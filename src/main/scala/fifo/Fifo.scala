package fifo

import csv.BuySell.{BuySell, buy, sell}
import csv.Constants.USDEur
import csv.Currency.{Currency, eur}
import csv.OrderType.{OrderType, lmt}
import csv.{Excel, Row}

import java.math.{BigDecimal, RoundingMode}
import scala.annotation.tailrec
import scala.collection.immutable.HashMap

final case class Bestand(quantity: BigDecimal,
                         netCash: BigDecimal,
                         orderType: OrderType,
                         ibCommission: BigDecimal,
                         tradePrice: BigDecimal,
                         taxes: String,
                         tradeMoney: BigDecimal,
                         changeInQuantity: String,
                         tradeDate: String,
                         dateTime: String,
                         buySell: BuySell,
                         ibCommissionCurrency: Currency,
                         currencyPrimary: Currency,
                         orderTime: String,
                         symbol: String,
                         netCashEuro: BigDecimal){

}

case class AufgebrauchteBestaende(verbleibend: List[Bestand], aufgebraucht: List[Bestand], neueAnzahl: BigDecimal){

}

case class State(excelEintraege: List[Excel], eur: BigDecimal, bestand: Map[String, List[Bestand]]){

}

object Fifo {

  val scale = 8

  type CurrencyMap = Map[String, BigDecimal]

  /**
   *
   * Achtung, hier wird nicht verrechnet mit bestehenden Positionen, sonst gibt das nur Kuddelmuddel.
   *
   * @param newQuantity die angepasste Anzahl an Aktien (nachdem nachgeguckt wurde, ob wir eine Negativposition bereits haben
   * @param row die aktuell betrachtete Transaktion
   * @return
   */
  def rowToBestand (newQuantity: BigDecimal, row: Row): Bestand = {
    val fraction = newQuantity.divide(row.quantity, scale, RoundingMode.HALF_UP)
    val newNetCash = if(row.quantity == newQuantity) row.netCash else row.netCash.multiply(fraction)
    val updatedRow = row.copy(quantity = newQuantity, netCash = newNetCash)
    fromRow(updatedRow, null)
  }

  def ermittleEuroErloese(aufgebrauchteBestaende: List[Bestand]): BigDecimal = {
    val euros: List[BigDecimal] = aufgebrauchteBestaende.map(b => b.netCashEuro)
    if (aufgebrauchteBestaende.isEmpty) BigDecimal.ZERO else euros.reduce((x, y) => x.add(y))
  }

  def ermittleAufgebrauchtePositionen(aufgebrauchteBestaende: List[Bestand]): BigDecimal = {
    if (aufgebrauchteBestaende.isEmpty) BigDecimal.ZERO else aufgebrauchteBestaende.map(b => b.quantity).reduce((x, y) => x.add(y))
  }

  def rowToExcelzeile(euroCash: BigDecimal, aufgebrauchteBestaende: List[Bestand], row: Row): Excel = {
    val anzahlVerbrauchterPositionen = if (aufgebrauchteBestaende.isEmpty) BigDecimal.ZERO else ermittleAufgebrauchtePositionen(aufgebrauchteBestaende)
    val euroCashAnteilig = euroCash.multiply(anzahlVerbrauchterPositionen.divide(row.quantity, scale, RoundingMode.HALF_UP).abs())
    // Wir schreiben nur tatsächlich realisierte Gewinne auf, andere Erlöse kommen erst später
    val nettoErloes = if (aufgebrauchteBestaende.isEmpty) BigDecimal.ZERO else euroCashAnteilig.add(ermittleEuroErloese(aufgebrauchteBestaende))
    Excel(row.dateTime, row.symbol, row.quantity, row.buySell, row.tradePrice, euroCash, nettoErloes, row.ibCommission)
  }

  def filtereBestand(buy: Boolean, item: Bestand): Boolean = {
    () match {
        case _ if buy && item.quantity.signum() < 0 =>  true
        case _ if !buy && item.quantity.signum() > 0 => true
        case _                                                         => false
    }
  }

  def obtainTradeDate(dateTime: String): String = {
    dateTime.substring(0, 4) + "-" + dateTime.substring(4, 6) + "-" + dateTime.substring(6, 8)
  }

  def fromRow(row: Row, netCashEuro: BigDecimal): Bestand = {
    Bestand(row.quantity, row.netCash, row.orderType, row.ibCommission, row.tradePrice, row.taxes, row.tradeMoney, row.changeInQuantity,
      row.tradeDate, row.dateTime, row.buySell, row.ibCommissionCurrency, row.currencyPrimary, row. orderTime, row.symbol, netCashEuro)
  }

  def getNetCashEuro(currencyMap: CurrencyMap, netCash: BigDecimal, tradeDate: String, currency: Currency): BigDecimal = {
    val wechselkurs = currencyMap(obtainTradeDate(tradeDate))
    if (currency == eur) netCash else netCash.divide(wechselkurs, scale, RoundingMode.HALF_UP)
  }

  def netCashEuroHinzufuegen(currencyMap: CurrencyMap, bestand: Bestand): Bestand = {
    bestand.copy(netCashEuro = getNetCashEuro(currencyMap, bestand.netCash, bestand.tradeDate, bestand.currencyPrimary))
  }

  @tailrec def loopOverBestaende(currencyMap: CurrencyMap, firstEntry: Bestand, anzahl: BigDecimal, runner: AufgebrauchteBestaende): AufgebrauchteBestaende = {
    val entryQty = firstEntry.quantity
    val entryQtyAbs = firstEntry.quantity.abs()
    val anzahlAbs = anzahl.abs()
    () match {
      case _ if entryQtyAbs == anzahlAbs => runner.copy(aufgebraucht = runner.aufgebraucht.appendedAll(List(firstEntry)))
      case _ if entryQtyAbs.compareTo(anzahlAbs) > 0 =>
        val remaining = entryQty.add(anzahl)
        val verbleibendNetCashEuro = firstEntry.netCashEuro.multiply(remaining).divide(entryQty, scale, RoundingMode.HALF_UP)
        val verbleibendNetCash = firstEntry.netCash.multiply(remaining).divide(entryQty, scale, RoundingMode.HALF_UP)
        val aufgebrauchtNetCashEuro = firstEntry.netCashEuro.multiply(anzahl).divide(entryQty, scale, RoundingMode.HALF_UP).negate()
        val aufgebrauchtNetCash = firstEntry.netCash.multiply(anzahl).divide(entryQty, scale, RoundingMode.HALF_UP).negate()
        val verbleibenderEntry = firstEntry.copy(quantity = remaining, netCash = verbleibendNetCash, netCashEuro = verbleibendNetCashEuro)
        val aufgebrauchterEntry = firstEntry.copy(quantity = anzahl.negate(), netCash = aufgebrauchtNetCash,
          netCashEuro = aufgebrauchtNetCashEuro)
        runner.copy(verbleibend = runner.verbleibend.appendedAll(List(verbleibenderEntry)),
          aufgebraucht =  runner.aufgebraucht.appendedAll(List(aufgebrauchterEntry)))
      case _ if entryQtyAbs.compareTo(anzahlAbs) < 0 =>
        val neueAnzahl = anzahl.add(entryQty)
        val aufgebraucht2 = runner.aufgebraucht.appendedAll(List(firstEntry))
        if (runner.verbleibend.isEmpty) runner.copy(aufgebraucht = aufgebraucht2,
          verbleibend = List(), neueAnzahl = neueAnzahl) else
          loopOverBestaende(currencyMap, netCashEuroHinzufuegen(currencyMap, runner.verbleibend.head), neueAnzahl,
            runner.copy(aufgebraucht = aufgebraucht2,
                        verbleibend = if(runner.verbleibend.isEmpty) List() else runner.verbleibend.tail))
    }
  }

  def ermittleBestaende(state: State, symbol: String, anzahl: BigDecimal, currencyMap: CurrencyMap): AufgebrauchteBestaende = {
    val buy = anzahl.signum() > 0
    val alterBestand = state.bestand.getOrElse(symbol, List())
    val bestandDerSchrumpfenKoennte = alterBestand.filter(item => filtereBestand(buy, item))
    val bestandDerWachsenKoennte = alterBestand.filter(item => filtereBestand(!buy, item))
    val reduzierterBestand: AufgebrauchteBestaende = if (bestandDerSchrumpfenKoennte.isEmpty) AufgebrauchteBestaende(List(), List(), anzahl)
        else {
          val bestandDerSchrumpfenKoennte2 = if(bestandDerSchrumpfenKoennte.isEmpty) List() else bestandDerSchrumpfenKoennte.tail
          val runner = AufgebrauchteBestaende(bestandDerSchrumpfenKoennte2, List(), BigDecimal.ZERO)
          loopOverBestaende(currencyMap, netCashEuroHinzufuegen(currencyMap, bestandDerSchrumpfenKoennte.head), anzahl, runner)
    }
    reduzierterBestand.copy(verbleibend = bestandDerWachsenKoennte.appendedAll(reduzierterBestand.verbleibend))
  }

  def buyOrSellEur(currencyMap: CurrencyMap, state: State, row: Row): State = {
    val bestaende = ermittleBestaende(state, row.symbol, row.quantity, currencyMap)
    val bestandNeuSymbol = if (bestaende.neueAnzahl != BigDecimal.ZERO)
      bestaende.verbleibend.appendedAll(List(rowToBestand(bestaende.neueAnzahl, row))) else bestaende.verbleibend
    val cash = row.netCash
    val excel = rowToExcelzeile(cash, bestaende.aufgebraucht, row)
    state.copy(state.excelEintraege.appendedAll(List(excel)), state.eur.add(cash), state.bestand + (row.symbol -> bestandNeuSymbol))
  }

  def fakeUsd(euroAmount: BigDecimal, amount: BigDecimal, inverserWechselkurs: BigDecimal, row: Row, buyOrSell: Boolean): Row = {
    Row(tradeMoney = amount, orderTime = row.orderTime, changeInQuantity = "0", buySell = if (buyOrSell) buy else sell,
      tradePrice = inverserWechselkurs,
      ibCommissionCurrency = eur, ibCommission = BigDecimal.ZERO, taxes = "0", currencyPrimary = eur, tradeDate = row.tradeDate,
      orderType = lmt, netCash = euroAmount, dateTime = row.dateTime, quantity = amount, symbol = USDEur)
  }

  def buyOrSellUsd(currencyMap: CurrencyMap, state0: State, row: Row): State = {
    val cash = row.netCash
    val wechselkurs = currencyMap(obtainTradeDate(row.tradeDate))
    val eurCash = cash.divide(wechselkurs, scale , RoundingMode.HALF_UP)
    // Hier die umgekehrte Operation auf der Usd-Reserve
    val fakeRow = fakeUsd(eurCash.negate(), cash, BigDecimal.ONE.divide(wechselkurs, 4, RoundingMode.HALF_UP), row, row.buySell == sell)
    val state = buyOrSellEur(currencyMap, state0, fakeRow)
    val bestaende = ermittleBestaende(state, row.symbol, row.quantity, currencyMap)
    val bestandNeuSymbol = if (bestaende.neueAnzahl != BigDecimal.ZERO)
      bestaende.verbleibend.appendedAll(List(rowToBestand(bestaende.neueAnzahl, row))) else bestaende.verbleibend
    val excel = rowToExcelzeile(eurCash, bestaende.aufgebraucht, row)
    state.copy(state.excelEintraege.appendedAll(List(excel)), state.eur.add(eurCash), state.bestand + (row.symbol -> bestandNeuSymbol))
  }

  def transact(currencyMap: CurrencyMap, state: State, row: Row): State = {
    if(row.currencyPrimary == eur) buyOrSellEur(currencyMap, state, row) else buyOrSellUsd(currencyMap, state, row)
  }

  def loopOverIbRows(currencyMap: CurrencyMap, rows: List[Row], eur: BigDecimal): State = {
    val startingState = State(List(), eur, new HashMap())
    rows.foldLeft(startingState)((state, row) => transact(currencyMap, state, row))
  }


}
