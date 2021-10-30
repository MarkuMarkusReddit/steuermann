package etf

import java.math.BigDecimal

/**
 * Experimentelle Datei (noch nicht benutzt), um die Vorabpauschale von ETFs zu berechnen.
 */
object ETF {

  //0.52% -> 0.36%
  //26.38%
  def berechneSteuer(ausschuettung: BigDecimal,
                     abgeltungssteuersatz: BigDecimal,
                     wertJahresanfangOderKauf: BigDecimal,
                     wertJahresendeOderVerkauf: BigDecimal,
                     basiszins: BigDecimal): BigDecimal = {
    val teilfreistellungsquote = new BigDecimal("0.7")
    val basisertrag = basiszins.multiply(wertJahresanfangOderKauf).multiply(teilfreistellungsquote);
    val wertzuwachs = wertJahresendeOderVerkauf.add(wertJahresanfangOderKauf.negate()).max(BigDecimal.ZERO)
    val grundlageVorabpauschale = basisertrag.min(wertzuwachs)
    val vorabpauschale = BigDecimal.ZERO.max(grundlageVorabpauschale.add(ausschuettung.negate()))

    val steuergrundlage = if (vorabpauschale.compareTo(BigDecimal.ZERO) > 0) vorabpauschale else ausschuettung
    steuergrundlage.multiply(teilfreistellungsquote).multiply(abgeltungssteuersatz)
  }

  // 369.32
  berechneSteuer(new BigDecimal("2000"), new BigDecimal("0.2638"), new BigDecimal("100000"), new BigDecimal("107000"),new BigDecimal("0.0052"))

  // 30.28
  berechneSteuer(new BigDecimal("200"), new BigDecimal("0.2638"), new BigDecimal("100000"), new BigDecimal("107000"),new BigDecimal("0.0052"))

  // 67.22
  berechneSteuer(BigDecimal.ZERO, new BigDecimal("0.2638"), new BigDecimal("100000"), new BigDecimal("107000"),new BigDecimal("0.0052"))

  // 18.47
  berechneSteuer(new BigDecimal("100"), new BigDecimal("0.2638"), new BigDecimal("100000"), new BigDecimal("100200"),new BigDecimal("0.0052"))

  // 369.32
  berechneSteuer(new BigDecimal("2000"), new BigDecimal("0.2638"), new BigDecimal("100000"), new BigDecimal("100200"),new BigDecimal("0.0052"))

  // 0
  val verlust = berechneSteuer(BigDecimal.ZERO, new BigDecimal("0.2638"), new BigDecimal("100000"), new BigDecimal("90000"),new BigDecimal("0.0052"))
}
