package fpinscala.state

object state {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  RNG.ints(5)(new RNG.Simple(42))                 //> res0: (List[Int], fpinscala.state.RNG) = (List(16159453, -1281479697, -34030
                                                  //| 5902, -2015756020, 1770001318),Simple(115998806404289))
	RNG.ints2(5)(new RNG.Simple(42))          //> res1: (List[Int], fpinscala.state.RNG) = (List(1770001318, -2015756020, -340
                                                  //| 305902, -1281479697, 16159453),Simple(115998806404289))

	RNG._ints(5)(new RNG.Simple(42))          //> res2: (List[Int], fpinscala.state.RNG) = (List(16159453, -1281479697, -34030
                                                  //| 5902, -2015756020, 1770001318),Simple(115998806404289))

	Candy.simulateMachine(List(Coin, Turn, Turn, Turn, Coin, Coin, Turn, Coin, Coin, Turn)).run(Machine(false, 5, 5))
                                                  //> res3: ((Int, Int), fpinscala.state.Machine) = ((7,2),Machine(true,2,7))

	Candy.simulateMachine(List(Coin, Coin, Coin, Coin)).run(Machine(false, 0, 1))
                                                  //> res4: ((Int, Int), fpinscala.state.Machine) = ((1,0),Machine(false,0,1))

	Candy.simulateMachine(List(Turn, Turn, Turn, Turn)).run(Machine(false, 1, 1))
                                                  //> res5: ((Int, Int), fpinscala.state.Machine) = ((1,0),Machine(true,0,1))

	State.get.flatMap({ (x:Int) => new State((s:Int) => (s, s)) }).run(5)
                                                  //> res6: (Int, Int) = (5,5)

	State[Int, Int](s => (s, s)).flatMap { x => new State[Int, Int]( s => (2 * s, 3 * s)) }.run(4)
                                                  //> res7: (Int, Int) = (8,12)

	State.modify[Machine] { Candy.update.apply(Turn) }.run(Machine(false, 5, 5))
                                                  //> res8: (Unit, fpinscala.state.Machine) = ((),Machine(true,4,5))
}