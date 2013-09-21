// Listing 8.8 The Semantic Model (AST) for the SSI DSL

package org.edla.study.parsing.ch8.trading.ssi

object SSI_AST {
  type Market = String
  type Security = String
  type CustomerCode = String
  type BrokerCode = String
  type AccountNo = String
  type Bank = String

  trait SettlementModeRule
  case class SettleInternal(accountNo: AccountNo)
    extends SettlementModeRule
  case class SettleExternal(bank: Bank, accountNo: AccountNo)
    extends SettlementModeRule

  trait SettleCashSecurityRule
  case object SettleCash extends SettleCashSecurityRule
  case object SettleSecurity extends SettleCashSecurityRule

  trait SettlementRule
  case class SettleCashSecuritySeparate(
    set: List[(SettleCashSecurityRule, SettlementModeRule)])
      extends SettlementRule
  case class SettleAll(sm: SettlementModeRule) extends SettlementRule

  trait CounterpartyRule
  case class Customer(code: CustomerCode) extends CounterpartyRule
  case class Broker(code: BrokerCode) extends CounterpartyRule

  case class TradeTypeRule(cpt: CounterpartyRule,
                           mkt: Option[Market], sec: Option[Security],
                           tradingAccount: Option[AccountNo])

  case class StandingRule(ttr: TradeTypeRule,
                          str: SettlementRule)

  case class StandingRules(rules: List[StandingRule])
}
