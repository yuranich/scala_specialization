package forcomp

import forcomp.Anagrams.wordOccurrences

/**
  * Created by yuranich on 11.07.2017.
  */
object SomeTesting extends App {
  val word = "abcdacee"
//  val res = Anagrams.dictionary.map(w => (wordOccurrences(w), w)).groupBy(k => k._1).map(x => (x._1, x._2.map(e => e._2)))
//  res.foreach(println)
  val sentence = List("Linux", "rulez")
  println(Anagrams.sentenceAnagrams(sentence))
}
