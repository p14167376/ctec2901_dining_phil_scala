object Phil02
{
  import java.util.concurrent._
  
  val DELAY1 = 1100
  def inc(i: Int) = (i+1)%5
  def dec(i: Int) = (i+4)%5
  
  val fork    = Array.fill(5) {new Semaphore(1,true)}
  val waiter  = new Semaphore(4,true)

  val ESC_CHAR = "\u001b"
  val CSI_CHARS = ESC_CHAR + "["

  val printer = new Semaphore(1,true)
  val chairs = Array.fill[Int](5)(0)
  val forkowner = Array.fill[Int](5)(-1)
  
	def csi_clearscreen()
	{
		print (CSI_CHARS + "2J")
	}
  
	def csi_setcursorpos (x: Int, y: Int)
	{
		print (CSI_CHARS + x + ";" + y + "H")
	}

	def printchairstatus (i: Int)
	{
		if (forkowner(i) == dec(i))
			print ("--f  ")
		else if (forkowner(i) == i)
			print ("  f--")
		else
			print ("  f  ")

		if (chairs(i) == 0)
			print ("_")
		else
			print (i+1)
	}

	def printchairstatus2 (i: Int)
	{
		if (forkowner(i) == dec(i))
			print ("-f   ")
		else if (forkowner(i) == i)
			print ("   f-")
		else
			print ("  f  ")

		if (chairs(i) == 0)
			print ("_")
		else
			print (i+1)
	}

	def printstatusline (x: Int, y: Int)
	{
		csi_setcursorpos (x, y)
		print ("STATUS[ ")
		for (i <- chairs.indices)
		{
			printchairstatus (i)
		}
		println (" ]")
	}

	def printstatusline2 (x: Int, y: Int)
	{
		csi_setcursorpos (x, y)
		print ("STATUS[ ")
		for (i <- chairs.indices)
		{
			printchairstatus2 (i)
		}
		println (" ]")
	}

/*

        (4)---f---(3)
       /             \   
      /               \
     f                 f
     |                 |
     |                 |
    (5)               (2)
       \             /
        \           /
         f---(1)---f    
*/
	def printchairdiagram (i: Int)
	{
		if (chairs(i) == 1) print ("." + (i+1) + ".")
		else if (chairs(i) == 2) print ("(" + (i+1) + ")")
		else print (" _ ")
	}

	def printstatusdiagram (x: Int, y: Int)
	{
		csi_setcursorpos (x, y)
		print ("    ")
		printchairdiagram (3)
		if (forkowner(3) == 3) print ("--f    ")
		else if (forkowner(3) == 2) print ("    f--")
		else print ("   f   ")
		printchairdiagram (2)

		csi_setcursorpos (x+1, y)
        print ("   ")
        if (forkowner(4) == 3) print ("/")
		else print (" ")
		print ("             ")
        if (forkowner(2) == 2) print ("\\")
		else print (" ")

		csi_setcursorpos (x+2, y)
        print ("  ")
        if (forkowner(4) == 3) print ("f")
		else print (" ")
		print ("               ")
        if (forkowner(2) == 2) print ("f")
		else print (" ")

		csi_setcursorpos (x+3, y)
        print (" ")
        if (forkowner(4) == 3) print (" ")
        else if (forkowner(4) == 4) print (" ")
		else print ("f")
		print ("                 ")
        if (forkowner(2) == 2) print (" ")
        else if (forkowner(2) == 1) print (" ")
		else print ("f")

		csi_setcursorpos (x+4, y)
        print (" ")
        if (forkowner(4) == 4) print ("f")
		else print (" ")
		print ("                 ")
        if (forkowner(2) == 1) print ("f")
		else print (" ")

		csi_setcursorpos (x+5, y)
        print (" ")
        if (forkowner(4) == 4) print ("|")
		else print (" ")
		print ("                 ")
        if (forkowner(2) == 1) print ("|")
		else print (" ")

		csi_setcursorpos (x+6, y)
		printchairdiagram (4)
		print ("               ")
		printchairdiagram (1)

		csi_setcursorpos (x+7, y)
        print ("   ")
        if (forkowner(0) == 4) print ("\\")
		else print (" ")
		print ("             ")
        if (forkowner(1) == 1) print ("/")
		else print (" ")

		csi_setcursorpos (x+8, y)
        print ("    ")
        if (forkowner(0) == 4) print ("f")
		else print (" ")
		print ("           ")
        if (forkowner(1) == 1) print ("f")
		else print (" ")

		csi_setcursorpos (x+9, y)
		print ("      ")
		if (forkowner(0) == 4) print ("    ")
		else if (forkowner(0) == 0) print (" f--")
		else print ("f   ")
		printchairdiagram (0)
		if (forkowner(1) == 0) print ("--f ")
		else if (forkowner(1) == 1) print ("    ")
		else print ("   f")
	}

	def printstatus()
	{
		printer.acquire()
		printstatusline(1,1)
		printstatusline2(3,1)
		printstatusdiagram(6,10)
		csi_setcursorpos (24, 1)
		printer.release()
	}

  def philosopher(i: Int): Unit = {
                                                waiter.acquire()
												chairs(i) = 1
												printstatus()
    
    //println("P" + i + " sitting down")
    Thread.sleep(DELAY1)
                                                fork(i).acquire()
												forkowner(i) = i
												printstatus()
    //println("P" + i + " picked up f" + i)
    Thread.sleep(DELAY1)
                                                fork(inc(i)).acquire()
												forkowner(inc(i)) = i
												printstatus()
    //println("P" + i + " picked up f" + inc(i))
    Thread.sleep(DELAY1)
    //println("P" + i + " eating")
    Thread.sleep(DELAY1)
												chairs(i) = 2
												forkowner(i) = -1
												printstatus()
                                                fork(i).release()
    //println("P" + i + " put down  f" + i)
    Thread.sleep(DELAY1)
												forkowner(inc(i)) = -1
												printstatus()
                                                fork(inc(i)).release()
    //println("P" + i + " put down  f" + inc(i))
    Thread.sleep(DELAY1)
    //println("P" + i + " getting up")
    Thread.sleep(DELAY1)
    //println("P" + i + " thinking")
    Thread.sleep(DELAY1)
												chairs(i) = 0
												printstatus()
                                                waiter.release()
                                                philosopher(i)
  }
  
  val philosophers = Array.tabulate(5) { p =>
    new Thread {
      override def run {
        philosopher(p)
        }
      }
    }
  
  def main(args: Array[String]) {
	csi_clearscreen()
    philosophers.foreach(_.start)
//    philosophers.foreach(_.join)  // not needed as threads run indefinitely
  }
}
