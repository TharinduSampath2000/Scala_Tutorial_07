case class Account(_id: Int, bal: Double) {
  def id: Int = _id;

  var balance = bal;

  def addBal(amount: Double): Any = {
    balance += amount;
  }

  def subBal(amount: Double): Any = {
    balance -= amount;
  }

  def getBal: Double = balance;

  def transfer(amount: Double, toAccount: Account): Any = {
    this.subBal(amount);
    toAccount.addBal(amount);
  }

  override def toString(): String = s"$id : $balance"
}

class Bank(accounts: List[Account]) {

  def printAllAccounts(): Any = {
    accounts.map(x => {
      println(x);
    });
  }

  def negativeBalance(): List[Account] = accounts.filter(x => x.bal < 0.0)

  def allAccountBalance: Double = {
    var sum: Double = 0.0;
    accounts.map(x => sum += x.bal);
    sum
  };

  def afterInterest: Double = {
    var sum: Double = 0.0;

    accounts.map(x =>
      x match {
        case a if (a.bal >= 0) => sum += (a.bal + a.bal * .05);
        case b                 => sum += (b.bal + b.bal * .1);
      }
    )
    sum
  }
}

object Q3and4 extends App {

  var account_1 = Account(1, 25000.0)
  var account_2 = Account(2, 10000.0)

  //Q3
  println(s"account 1 : $account_1")
  println(s"account 2 : $account_2")
  
  println("Transferring 1000 from 2 to 1 ")

  var transfer_value = account_2.transfer(1000.0, account_1);

  println(s"account 1 : $account_1")
  println(s"account 2 : $account_2")

  var bank: Bank = Bank(
    List[Account](
      Account(1, 5000.0),
      Account(2, -1000.0),
      Account(3, 0.0),
      Account(4, 2000.0),
      Account(5, -300.0),
    )
  )
  
  //Q4
  println("\nBank Accounts: ")
  bank.printAllAccounts()

  println("\n************4.1**************")
  println("Accounts with negative balance")
  bank.negativeBalance().map(x => println(x))

  println("\n************4.2**************")
  println(s"Sum of balances of all Accounts = ${bank.allAccountBalance}");

  println("\n************4.3**************")
  println(s"Sum of balances of all Accounts  after applying interest= ${bank.afterInterest}");
}

