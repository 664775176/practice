
case class Student(p_id : Int, p_sex : String, p_name: String, p_class_id : Int,
                   p_chineseScore : Int, p_mathScore : Int, p_englishScore :Int) {
  val studentId : Int  = p_id    //学生的标识
  val sex : String = p_sex  //性别
  val studentName : String = p_name  //姓名
  val classId : Int = p_class_id     //所在班级
  val chineseScore :Int = p_chineseScore  //语文分数
  val mathScore : Int = p_mathScore   //数学分数
  val englishScore : Int = p_englishScore   //英语分数
}

object Practice1 {

  def main(args: Array[String]): Unit = {
    //初始化
    val students = init
    val group = students.groupBy { (student) => student.classId }

    group.keys.foreach { i =>
      val scoreList = group(i)
      //总分前5名
      val totalList = scoreListFun(scoreList, "Total")
      print("班级：" +  i + " 总分前5名为：")
      scoreSortFun(totalList,5)
      //语文分前5名
      val chineseList = scoreListFun(scoreList, "Chinese")
      print("班级：" +  i + " 语文分前5名为：")
      scoreSortFun(chineseList,5)
      //语文等级比例
      print("班级：" +  i + " 语文各等级人数比例为：")
      subjectRadioFun(chineseList)
      //数学分前5名
      val mathList = scoreListFun(scoreList, "Math")
      print("班级：" +  i + " 数学分前5名为：")
      scoreSortFun(mathList,5)
      //数学等级比例
      print("班级：" +  i + " 数学各等级人数比例为：")
      subjectRadioFun(mathList)
      //英语分前5名
      val englishList = scoreListFun(scoreList, "English")
      print("班级：" +  i + " 英语分前5名为：")
      scoreSortFun(englishList,5)
      //英语等级比例
      print("班级：" +  i + " 英语各等级人数比例为：")
      subjectRadioFun(englishList)

    }
    val totalList = nameListFun(students, "Total")
    print("全年级总分前20名为：")
    val score = totalList.sortWith({case(s1, s2) => s1._2 > s2._2})
    val slicedScore = score.slice(0, 20)
    for (elem <- slicedScore) {
      print(elem._1 + ",总分:" + elem._2 + "; ")
    }
    println()

    val group1 = students.groupBy { case (student) => student.sex}
    group1.keys.foreach { i =>
      val scoreList = group1(i)
      //语文状元
      val chineseList = nameListFun(scoreList, "Chinese")
      val chineseScore = chineseList.maxBy({case(name, score ) => score })
      println(i + "语文状元为：" + chineseScore._1 + ", 分数为：" + chineseScore._2)
      //数学状元
      val mathList = nameListFun(scoreList, "Math")
      val mathScore = mathList.maxBy({case(name, score ) => score })
      println(i + "数学状元为：" + mathScore._1 + ", 分数为：" + mathScore._2)
      //英语状元
      val englishList = nameListFun(scoreList, "English")
      val englishScore = englishList.maxBy({case(name, score ) => score })
      println(i + "英语状元为：" + englishScore._1 + ", 分数为：" + englishScore._2)
    }

  }

  def scoreListFun(students: List[Student], subjectName: String):  List[Int] = subjectName match {
    case "Total" =>
      students.map(e => e.chineseScore + e.mathScore + e.englishScore)
    case "Chinese" =>
      students.map(e => e.chineseScore)
    case "Math" =>
      students.map(e => e.mathScore)
    case "English" =>
      students.map(e => e.englishScore)
    case _ => throw new RuntimeException("错误的学科")
  }

  def scoreSortFun(scoreList: List[Int], num: Int): Unit = {
    val score = scoreList.sortWith { (student1, student2) => student1 > student2 }
    val slicedScore = score.slice(0, num)
    for (elem <- slicedScore) {
      print(elem + " ")
    }
    println()
  }

  def subjectRadioFun(scoreList: List[Int]): Unit ={
    val radio1 = scoreList.filter(_ >=90).length
    val radio2 = scoreList.filter(_ >=75).filter(_ <90).length
    val radio3 = scoreList.filter(_ >=60).filter(_ <75).length
    val radio4 = scoreList.filter(_ < 60).length
    println( radio1 +":" +radio2 + ":" + radio3 +":" + radio4)
  }
  def nameListFun(students: List[Student], subjectName: String):  List[(String, Int)] = subjectName match {
    case "Total" =>
      students.map(e => (e.studentName, e.chineseScore + e.mathScore + e.englishScore))
    case "Chinese" =>
      students.map(e => (e.studentName, e.chineseScore))
    case "Math" =>
      students.map(e => (e.studentName, e.mathScore))
    case "English" =>
      students.map(e => (e.studentName, e.englishScore))
    case _ => throw new RuntimeException("错误的学科")
  }
  def init :List[Student]=  {
    val student1 =  Student(1, "Man", "S1", 1, 90, 80, 82)
    val student2 =  Student(2, "Woman", "S2", 1, 52, 83, 56)
    val student3 =  Student(3, "Man", "S3", 1, 98, 64, 88)
    val student4 =  Student(4, "Woman", "S4", 1, 68, 45, 74)
    val student5 =  Student(5, "Woman", "S5", 1, 98, 56, 64)
    val student6 =  Student(6, "Man", "S6", 2, 90, 67, 64)
    val student7 =  Student(7, "Woman", "S7", 2, 78, 78, 88)
    val student8 =  Student(8, "Man", "S8", 2, 68, 89, 63)
    val student9 =  Student(9, "Man", "S9", 2, 88, 97, 67)
    val student10 =  Student(10, "Woman", "S10", 2, 58, 86, 76)
    val student11 =  Student(11, "Man", "S11", 2, 78, 75, 77)
    val student12 =  Student(12, "Man", "S12", 1, 98, 46, 78)

    val student13 =  Student(13, "Man", "S13", 1, 70, 90, 62)
    val student14 =  Student(14, "Woman", "S14", 1, 55, 87, 76)
    val student15 =  Student(15, "Man", "S15", 1, 98, 67, 88)
    val student16 =  Student(16, "Woman", "S16", 1, 88, 55, 77)
    val student17 =  Student(17, "Woman", "S17", 1, 88, 56, 64)
    val student18 =  Student(18, "Man", "S18", 2, 99, 68, 64)
    val student19 =  Student(19, "Woman", "S19", 2, 78, 75, 88)
    val student20 =  Student(20, "Man", "S20", 2, 68, 86, 63)
    val student21 =  Student(21, "Man", "S21", 2, 87, 96, 68)
    val student22 =  Student(22, "Woman", "S22", 2, 55, 85, 73)
    val student23 =  Student(23, "Man", "S23", 2, 78, 75, 70)
    val student24 =  Student(24, "Man", "S24", 1, 90, 40, 75)
    val students: List[Student] = List(student1, student2, student3, student4, student5, student6, student7, student8, student9, student10, student11, student12,
      student13, student14, student15, student16, student17, student18, student19, student20, student21, student22, student23, student24)
    students
  }
}
