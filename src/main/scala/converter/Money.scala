package converter
import converter.Currencies.SupportedCurrencies
import converter.Errors.{UnsupportedCurrencyException, WrongCurrencyException, MoneyAmountShouldBePositiveException}

case class Money private (amount: BigDecimal, currency: String) {
  def +(other: Money): Money = {
    if (! isSameCurrency(other)) { throw new WrongCurrencyException }
    Money(amount + other.amount, currency)
  }
  def -(other: Money): Money = {
    if (! isSameCurrency(other)) { throw new WrongCurrencyException }
    Money(amount - other.amount, currency)
  }
  def isSameCurrency(other: Money): Boolean = { currency == other.currency }
}

object Money {
  def apply(amount: BigDecimal, currency: String): Money = {
    if (amount < 0) {
      throw new MoneyAmountShouldBePositiveException
    }

    if (!SupportedCurrencies.contains(currency)) {
      throw new UnsupportedCurrencyException
    }

    new Money(amount, currency)
  }
}
