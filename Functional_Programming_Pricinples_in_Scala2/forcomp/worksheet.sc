

object Experiment {
    def wordOccurances(word: String) = word.groupBy(c => c).transform((c, s) => s.length)
    wordOccurances("somethings")

    def sentenceOccurances(sentence: List[String]): List[(Char,Int)] = {
        sentence.flatMap(wordOccurances)
    }
    sentenceOccurances(List("somethings", "fishy"))
}