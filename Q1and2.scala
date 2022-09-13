import scala.compiletime.testing.Error.apply

object forGCD {
  def gcd(a: Int, b: Int): Int = { b match {
      case 0            => a;
      case x if (x > a) => gcd(b, a);
      case _            => gcd(b, a % b);
    }
  }
}

case class Rational(numerator: Int, denominator: Int) {
  if (denominator == 0) {
    throw Error("0 Denominator detected");
  }

  def numer: Int = numerator;
  def denom: Int = denominator;

  override def toString(): String = {
    if ((numer < 0 && denom > 0) || (numer > 0 && denom < 0)) {
      s"-${numer.abs}/${denom.abs}";
    } else {
      s"${numer.abs}/${denom.abs}";
    }
  }

  def neg: Rational = {
    new Rational(-1 * numer, denom).simplify
  }

  def +(that: Rational) = {
    new Rational(
      this.denom * that.numer + that.denom * this.numer,
      this.denom * that.denom
    ).simplify
  }

  def -(that: Rational) = {
    new Rational(
      this.denom * that.numer - that.denom * this.numer,
      this.denom * that.denom
    ).simplify
  }

  def simplify: Rational = {
    var a = numerator.abs;
    var b = denominator.abs;
    var gcd = forGCD.gcd(a, b);

    while (gcd != 1) {
      a = a / gcd;
      b = b / gcd;
      gcd = forGCD.gcd(a, b);
    }

    ((numer < 0 && denom > 0) || (numer > 0 && denom < 0)) match {
      case true  => Rational(-1 * a, b);
      case false => Rational(a, b);
    }
  }
}

object Q1and2 extends App {

  var number_1: Rational = Rational(3, 5);
  var number_2: Rational = Rational(-1, 8);

  //Q1
  println(s"var number_1 = $number_1");
  println(s"number_1.neg = ${number_1.neg}");
  println(s"var number_2 = $number_2");
  println(s"number_2.neg = ${number_2.neg}");

  var number_3: Rational = Rational(3, 8);
  var number_4: Rational = Rational(2, 7);

  //Q2
  println(s"\nnumber_3 = $number_3");
  println(s"number_4 = $number_4");
  println(s"number_3 + number_4 = $number_3 + $number_4 = ${number_3 + number_4}");
  println(s"number_3 - number_4 = $number_3 - $number_4 = ${number_3 - number_4}");

}
