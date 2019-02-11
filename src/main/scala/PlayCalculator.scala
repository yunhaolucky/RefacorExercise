class PlayCalculator {
  def statement(invoice: Invoice, plays: Map[String, Play]): String = {
    var totalAmount = 0
    var volumeCredits = 0
    var result = s"Statement for ${invoice.customer}\n"
    def format = (i: Int) => { s"$i" }
    invoice.performances.foreach { performance =>
      {
        val play = plays(performance.playId)
        val thisAmount = amountFor(play, performance)

        // add volume credits
        volumeCredits = volumeCredits + Math.max(performance.audience - 30, 0)
        // add extra credit for every ten comedy attendees
        if ("comedy" == play.playType)
          volumeCredits = volumeCredits +
            Math.round(performance.audience / 5)

        result = result + s"${play.name}: ${format(thisAmount / 100)} (${performance.audience} seats)\\n"
        totalAmount = totalAmount + thisAmount
      }
    }
    result = result + s"Amount owed is ${format(totalAmount / 100)}\n"
    result = result + s"You earned ${volumeCredits} credits\n"
    return result
  }

  private[this] def amountFor(play: Play, performance: Performance): Int = {
    var result = 0
    play.playType match {
      case "tragedy" => {
        result = 40000
        if (performance.audience > 30) {
          result = result + 1000 * (performance.audience - 30)
        }
      }
       case "comedy" => {
        result = 30000
        if (performance.audience > 20) {
          result = result + 10000 + 500 * (performance.audience - 20)
        }
      }
      case _ => throw new Exception("unknown type")
    }
    result
  }

  case class Invoice(customer: String, performances: Seq[Performance])
  case class Performance(playId: String, audience: Int)
  case class Play(playType: String, name: String)
}
