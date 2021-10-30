package csv

import csv.BuySell.{BuySell, toGerman}
import csv.Currency.Currency
import csv.OrderType.OrderType
import com.github.tototoshi.csv._
import csv.Constants.{EurUSD, USDEur}
import csv.Csv.invertEurUsd

import java.math.{BigDecimal, RoundingMode}
import java.io.File
import java.text.{DecimalFormat, NumberFormat}

/**
 * Um die Währungsgewinne zu ermitteln, werden künstliche "Euro verkaufen" oder "Euro kaufen"-Trades eingefügt.
 */
object Constants {
  val EurUSD = "EUR.USD"
  val USDEur = "USD.EUR"
}

object CurrencyFormat extends DefaultCSVFormat {
  override val delimiter = ';'
}

object OrderType extends Enumeration {
  type OrderType = Value
  val lmt: OrderType = Value("LMT")
  val mkt: OrderType = Value("MKT")
}

object BuySell extends Enumeration {
  type BuySell = Value
  val buy: BuySell = Value("BUY")
  val sell: BuySell = Value("SELL")

  def toGerman(buySell: BuySell): String = {
    if(buySell == BuySell.buy)  "KAUF" else{"VERKAUF"}
  }
}

object Currency extends Enumeration {
  type Currency = Value
  val eur: Currency = Value("EUR")
  val usd: Currency = Value("USD")
}

final case class Excel(dateTime: String,
                 symbol: String,
                 quantity: BigDecimal,
                 buySell: BuySell,
                 tradePrice: BigDecimal,
                 euroKosten: BigDecimal,
                 euroErloesNetto: BigDecimal,
                 gebuehren: BigDecimal) {

}

final case class Row(quantity: BigDecimal,
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
               symbol: String
              ) {}

object Csv {
  val nfShort: DecimalFormat = initNfShort()
  val nfLong: DecimalFormat = initNfLong()

  def invertEurUsd(row: Row): Row = {
    if(row.symbol == EurUSD){
      Row(row.tradeMoney.negate(), row.quantity.add(row.ibCommission), row.orderType, row.ibCommission, new BigDecimal(1).divide(row.tradePrice, 2, RoundingMode.UP),
        row.taxes, row.quantity, row.changeInQuantity, row.tradeDate, row.dateTime, if (row.buySell == BuySell.buy) BuySell.sell else BuySell.buy,
        row.ibCommissionCurrency, if (row.currencyPrimary == Currency.eur) Currency.usd else Currency.eur, row.orderTime, USDEur
      )
    } else {
      row
    }
  }

  def initNfShort(): DecimalFormat = {
    val nfShort = java.text.NumberFormat.getNumberInstance(java.util.Locale.GERMANY).asInstanceOf[DecimalFormat]
    nfShort.applyPattern("#,##0.00")
    nfShort
  }

  def initNfLong(): DecimalFormat = {
    val nfShort = java.text.NumberFormat.getNumberInstance(java.util.Locale.GERMANY).asInstanceOf[DecimalFormat]
    nfShort.applyPattern("#,##0.0000")
    nfShort
  }

  def convertExcelsToStrings(excels: List[Excel]): List[List[String]] = {
    excels.map(excel => convertExcelToStrings(excel))
  }

  def convertExcelToStrings(excel: Excel): List[String] = {
    List(excel.dateTime, excel.symbol, nfShort.format(excel.quantity), toGerman(excel.buySell),
      nfLong.format(excel.tradePrice), nfShort.format(excel.euroKosten), nfShort.format(excel.euroErloesNetto), nfShort.format(excel.gebuehren))
  }

  def writeResultsBack(excel: List[Excel], filename: String): Unit = {
    val writer = CSVWriter.open(new File(filename))
    writer.writeAll(List(List("Zeit", "Symbol", "Anzahl", "Kauf/Verkauf", "Preis", "Eurokosten", "Erlös netto", "Gebühren (Landeswährung)")).appendedAll(convertExcelsToStrings(excel)))
  }
}

class Csv {

}

