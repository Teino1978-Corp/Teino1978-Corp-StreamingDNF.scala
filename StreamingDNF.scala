sealed trait Formula[A]
case class Var[A](a: A) extends Formula[A]
case class Not[A](fm: Formula[A]) extends Formula[A]
case class And[A](left: Formula[A], right: Formula[A]) extends Formula[A]
case class Or[A](left: Formula[A], right: Formula[A]) extends Formula[A]

type DNFSTREAM[A] = Stream[List[Formula[A]]]

def distribute[A](left: DNFSTREAM[A], right: DNFSTREAM[A]): DNFSTREAM[A] = 
  for (leftTerm <- left; rightTerm <- right) yield leftTerm ++ rightTerm

/* assumption: fm in negation normal form */
def streamingDNF[A](fm: Formula[A]): DNFSTREAM[A] = fm match {
  case And(left, right) => distribute(streamingDNF(left), streamingDNF(right))
  case Or(left, right) => streamingDNF(left) ++ streamingDNF(right)
  case lit => Stream(List(lit))
}

def genCnf(length: Int) = 
  (for (i <- 1 to 2*length by 2) yield Or(Var(i), Var(i+1))) reduce(And(_:Formula[Int],_:Formula[Int]))

/* try e.g. streamingDNF(genCnf(32)) which will create a stream of 2^32 terms :) */