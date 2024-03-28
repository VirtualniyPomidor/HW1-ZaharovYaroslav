package converter

import converter.Currencies.SupportedCurrencies
import converter.Errors.{UnsupportedCurrencyException, WrongCurrencyException}

class CurrencyConverter(ratesDictionary: Map[String, Map[String, BigDecimal]]) {
  def exchange(money: Money, toCurrencies: String): Money = {
    if (money.currency != toCurrencies) {
      val fromCur = money.currency
      val cur = ratesDictionary.getOrElse(fromCur, throw new UnsupportedCurrencyException).getOrElse(toCurrencies, throw new UnsupportedCurrencyException)
      val exchangedMoney = cur*money.amount
      Money(exchangedMoney, toCurrencies)
    }
    else { throw new WrongCurrencyException }
  }
}

object CurrencyConverter {
  def apply(ratesDictionary: Map[String, Map[String, BigDecimal]]): CurrencyConverter = {
    val fromCurrencies = ratesDictionary.keys
    val toCurrencies = ratesDictionary.values
    if (
      fromCurrencies.toSet.subsetOf(SupportedCurrencies) &&
        toCurrencies.forall(_.keys.toSet.subsetOf(SupportedCurrencies))
    )
      new CurrencyConverter(ratesDictionary)
    else throw new UnsupportedCurrencyException
  }
}
