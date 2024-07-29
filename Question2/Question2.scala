import scala.io.StdIn.readLine

object StudentRecords {

  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty"))
    } else if (marks < 0) {
      (false, Some("Marks should be a non-negative integer"))
    } else if (totalMarks <= 0) {
      (false, Some("Total possible marks should be a positive integer"))
    } else if (marks > totalMarks) {
      (false, Some("Marks should not exceed total possible marks"))
    } else {
      (true, None)
    }
  }

  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    def readName(): String = {
      println("Enter student's name:")
      readLine().trim
    }

    def readMarks(): Int = {
      println("Enter marks obtained:")
      readLine().trim.toIntOption.getOrElse {
        println("Invalid input. Please enter a valid non-negative integer for marks.")
        readMarks()
      }
    }

    def readTotalMarks(): Int = {
      println("Enter total possible marks:")
      readLine().trim.toIntOption.getOrElse {
        println("Invalid input. Please enter a valid positive integer for total possible marks.")
        readTotalMarks()
      }
    }

    val studentInfo = Iterator.continually {
      val name = readName()
      val marks = readMarks()
      val totalMarks = readTotalMarks()
      validateInput(name, marks, totalMarks) match {
        case (true, None) => (name, marks, totalMarks)
        case (false, Some(errorMessage)) =>
          println(s"Error: $errorMessage")
          null
      }
    }.find(_ != null).get

    val (name, marks, totalMarks) = studentInfo
    val percentage = (marks.toDouble / totalMarks) * 100
    val grade = percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }

    (name, marks, totalMarks, percentage, grade)
  }

  def printStudentRecord(student: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = student
    println(s"Student Record:")
    println(s"Name: $name")
    println(s"Marks Obtained: $marks")
    println(s"Total Possible Marks: $totalMarks")
    println(s"Percentage: %.2f%%".format(percentage))
    println(s"Grade: $grade")
  }

  def main(args: Array[String]): Unit = {
    val studentInfo = getStudentInfoWithRetry()
    printStudentRecord(studentInfo)
  }
}
