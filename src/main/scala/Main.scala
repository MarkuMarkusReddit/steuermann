import com.github.tototoshi.csv.CSVReader
import csv.Csv.invertEurUsd
import csv.{BuySell, Csv, Currency, CurrencyFormat, OrderType, Row}
import fifo.Fifo

import java.io.File
import java.math.BigDecimal
import java.text.NumberFormat

class Main {

}

object Main {

  val nf: NumberFormat = java.text.NumberFormat.getNumberInstance(java.util.Locale.GERMANY)

  /**
   *
   * @param fileName die Datei von IBKR
   * @return Liste von Trades
   */
  def orders(fileName: String): List[Row] = {
    val tradesReader: CSVReader = CSVReader.open(new File(fileName))
    var predecessorWasCancelled = false
    tradesReader.allWithHeaders()
      .filter(order => if(order.getOrElse("Buy/Sell", "").contains("(Ca.)")) {
        predecessorWasCancelled = true
        false
      } else if(predecessorWasCancelled){
        predecessorWasCancelled = false
        false
      } else {
        true
      })
      .map(order => Row(new BigDecimal(order.getOrElse("Quantity", null)),
      new BigDecimal(order.getOrElse("NetCash", null)),
      OrderType.withName(order.getOrElse("OrderType", null)),
      new BigDecimal(order.getOrElse("IBCommission", null)),
      new BigDecimal(order.getOrElse("TradePrice", null)),
      order.getOrElse("Taxes", null),
      new BigDecimal(order.getOrElse("TradeMoney", null)),
      order.getOrElse("ChangeInQuantity", null),
      order.getOrElse("TradeDate", null),
      order.getOrElse("DateTime", null),
      BuySell.withName(order.getOrElse("Buy/Sell", null)),
      Currency.withName(order.getOrElse("IBCommissionCurrency", null)),
      Currency.withName(order.getOrElse("CurrencyPrimary", null)),
      order.getOrElse("OrderTime", null),
      order.getOrElse("Symbol", null),
    ))
      .map(row => invertEurUsd(row))
      .sortBy(row => row.orderTime)
  }

  /**
   *
   * @param fileName die Datei
   * @return Wechselkurse
   */
  def currencyMap(fileName: String): Map[String, java.math.BigDecimal] = {

    val currencyReader: CSVReader = CSVReader.open(new File (fileName))(CurrencyFormat)
    // Filtert den unnötigen Header und die vielen nicht benötigten Kurse raus, sowie den Kram am Ende.
    val currencyRows: List[List[String]] = currencyReader.all().drop(6000).filter(item => item.head.startsWith("202") && item(1) != ".")

    currencyRows.map(row => Map(row.head -> new BigDecimal(nf.parse(row(1)).asInstanceOf[Double]))).fold(Map())((row1, row2) => row1 ++ row2)
  }

  def run(wechselkursDateiPfad: String, tradesIBKRCSV: String, startguthabenEUR: String, ausgabeDatei: String): Unit = {
    val cMap = currencyMap(wechselkursDateiPfad)
    val rows = orders(tradesIBKRCSV)
    val state = Fifo.loopOverIbRows(cMap, rows, new BigDecimal(startguthabenEUR))
    Csv.writeResultsBack(state.excelEintraege, ausgabeDatei)
  }

  def runUSD(wechselkursDateiPfad: String, tradesIBKRCSV: String, startguthabenEUR: String, ausgabeDatei: String): Unit = {
    val cMap = currencyMap(wechselkursDateiPfad)
    val rows = orders(tradesIBKRCSV)
    val state = Fifo.loopOverIbRows(cMap, rows, new BigDecimal(startguthabenEUR))
    Csv.writeResultsBack(state.excelEintraege.filter(item => item.symbol == "USD.EUR"), ausgabeDatei)
  }

  def runOptions(wechselkursDateiPfad: String, tradesIBKRCSV: String, startguthabenEUR: String, ausgabeDatei: String): Unit = {
    val cMap = currencyMap(wechselkursDateiPfad)
    val rows = orders(tradesIBKRCSV)
    val state = Fifo.loopOverIbRows(cMap, rows, new BigDecimal(startguthabenEUR))
    Csv.writeResultsBack(state.excelEintraege.filter(item => item.symbol contains  "  "), ausgabeDatei)
  }

  def runStocks(wechselkursDateiPfad: String, tradesIBKRCSV: String, startguthabenEUR: String, ausgabeDatei: String): Unit = {
    val cMap = currencyMap(wechselkursDateiPfad)
    val rows = orders(tradesIBKRCSV)
    val state = Fifo.loopOverIbRows(cMap, rows, new BigDecimal(startguthabenEUR))
    Csv.writeResultsBack(state.excelEintraege.filter(item => !(item.symbol contains  "  ") && item.symbol != "USD.EUR"), ausgabeDatei)
  }

  /**
   *
   * @param args Wechselkursdateipfad, Pfad für CSV von IBRK, Startguthaben EUR, Ausgabedateipfad
   */
  def main(args: Array[String]): Unit = {
    if(args.length > 4){
      if(args(4) == "usd"){
        runUSD(args(0), args(1), args(2), args(3))
      } else if(args(4) == "options"){
        runOptions(args(0), args(1), args(2), args(3))
      } else {
        runStocks(args(0), args(1), args(2), args(3))
      }
    } else {
      run(args(0), args(1), args(2), args(3))
    }
  }
}