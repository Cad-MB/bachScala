
package upmc.akka.culto

import math._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import akka.actor.{Props, Actor, ActorRef, ActorSystem}


abstract class ObjectMusical

abstract class SimpleObjectMusical() extends ObjectMusical

abstract class ComposeObjectMusical() extends ObjectMusical

case class Note(pitch: Int, dur: Int, vel: Int) extends SimpleObjectMusical

case class Rest(dur: Int) extends SimpleObjectMusical

case class Sequential(elements: List[ObjectMusical]) extends ComposeObjectMusical

case class Parallel(elements: List[ObjectMusical]) extends ComposeObjectMusical

case class MidiNote(pitch: Int, vel: Int, dur: Int, at: Int)

/////////////////////////////////////////////////

class BachActor extends Actor {

  //////////////////////////////////////////////////
  val remote =
    context.actorSelection("akka.tcp://Player@127.0.0.1:6000/user/Instrument")

  ////////////////////////////////////////////////

  def receive = {
    case "START" => {
      println("start")

      //Question 2
      val totalDuration = duration(exemple)
      println(s"Duree totale de l'exemple: $totalDuration ms")

      //Question 3
      println("Début du test des fonctions...")

      play(exemple)

      // test copy
      val copie = copy(exemple)
      println(s"Copie de l'exemple : $copie")

      // test note_count
      val nbNotes = note_count(exemple)
      println(s"Nombre de notes dans l'exemple : $nbNotes")
      assert(note_count(exemple) == note_count(copie))

      // test stretch
      val exempleEtire = stretch(exemple, 2.0)
      println(s"Exemple après étirement : $exempleEtire")

      // comparaison de durées
      println(s"Durée originale : ${duration(exemple)} ms")
      println(s"Durée étirée : ${duration(exempleEtire)} ms")

      // jouer l'exemple original et étiré
      play_midi(exemple, 0)
      play_midi(exempleEtire, duration(exemple))

      // Question 4

      // test transpose
      val exempleTranspose = transpose(exemple, 12)
      println(s"Exemple transposé : $exempleTranspose")
      play(exempleTranspose)

      // test retrograde
      val exempleRetrograde = retrograde(exemple)
      println(s"Exemple rétrograde : $exempleRetrograde")
      play(exempleRetrograde)

      // test mirror
      val exempleMiroir = mirror(exemple, 60)
      println(s"Exemple miroir : $exempleMiroir")
      play(exempleMiroir)

      // test repeat
      val exempleRepete = repeat(exemple, 3)
      println(s"Exemple répété : $exempleRepete")
      play(exempleRepete)

      // test canon
      val exempleCanon = canon(exemple, 1000)
      println(s"Exemple en canon : $exempleCanon")
      play(exempleCanon)

      // test concat
      val exempleConcat = concat(exemple, transpose(exemple, 12))
      println(s"Exemple concaténé : $exempleConcat")
      play(exempleConcat)

      //Question 5
      val testRepeat = repeat(exemple, 3)
      println(s"Test repeat (exemple répété 3 fois) : $testRepeat")
      play(testRepeat)

      val testCanon = canon(exemple, 1000)
      println(s"Test canon (exemple en canon avec 1000ms de décalage) : $testCanon")
      play(testCanon)

      val testConcat = concat(exemple, transpose(exemple, 12))
      println(s"Test concat (exemple concaténé avec sa transposition) : $testConcat")
      play(testConcat)

    }
  }

}

  /////////////////////////////////////////////////

  //Question 1
  val exemple = Parallel(List(
    Sequential(List(        // clé sol
      Note(60, 1000, 100),  // noire : c4 1000ms
      Note(62, 500, 100),   // croche : d4 500ms
      Note(64, 500, 100),   // croche : e4 500ms
      Rest(500),            // silence d'une croche
      Note(65, 1000, 100)   // noire : f4 1000ms
    )),
    Sequential(List(        // clé fa
      Note(48, 1000, 100),  // noire : c3 1000ms
      Rest(1000),           // silence d'une noire
      Note(50, 1000, 100),  // noire : d3 1000ms
      Rest(1000),           // silence d'une noire
      Note(52, 1000, 100)   // noire : e3 1000ms
    ))
  ))

  //Question 2

  // Calcule la duree d'un objet musical
  def duration(obj: ObjectMusical): Int =
    obj match {
      case Note(p, d, v) => d
      case Rest(d) => d
      case Sequential(l) => (l.map(duration)).foldLeft(0)(_ + _)
      case Parallel(l) => (l.map(duration)).foldLeft(0)(math.max)
    }


  //Question 3
  def play(obj: ObjectMusical): Unit = {
    play_midi(obj, 0)
  }

  def send_a_note(p: Int, d: Int, v: Int, at: Int): Unit = {
    remote ! MidiNote(p, v, d, at)
  }


  def play_midi(obj: ObjectMusical, at: Int): Unit =
    obj match {
      case Note(p, d, v) => send_a_note(p, d, v, at)
      case Rest(d) => Nil
      case Sequential(l) => {
        var date = at
        l.foreach(n => {
          play_midi(n, date); date = date + duration(n)
        })
      }
      case Parallel(l) => l.foreach(n => play_midi(n, at))
    }

   // Copy un objet musical
    def copy (obj:ObjectMusical):ObjectMusical =
    obj match {
      case Note(p,d,v) => Note(p,d,v)
      case Rest(d) => Rest(d)
      case Sequential (l) => Sequential (l.map(copy))
      case Parallel (l) => Parallel (l.map(copy))
    }

    // Compte le nombre de notes d'un objet musical
    def note_count (obj:ObjectMusical):Int =
    obj match {
      case Note(p,d,v) => 1
      case Parallel (l) => (l.map(note_count)).foldLeft(0)(_+_)
      case Sequential (l) => (l.map(note_count)).foldLeft(0)(_+_)
      case _ => 0
    }

    // Strech un objet musical par un factor fact
    def stretch (obj:ObjectMusical, fact:Double ):ObjectMusical =
    obj match {
      case Note(p,d,v) => Note(p,(d*fact).toInt,v)
      case Rest(d) => Rest((d*fact).toInt)
      case Parallel (l) => Parallel (l.map(stretch (_,fact)))
      case Sequential (l) => Sequential (l.map(stretch (_,fact)))
    }


  // Question 4

  // Transpose obj de n demitons
    def transpose (obj:ObjectMusical, n:Int ):ObjectMusical =
  obj match {
    case Note(p,d,v) => Note(p+n,d,v)
    case Rest(d) => Rest(d)
    case Parallel (l) => Parallel (l.map(transpose (_,n)))
    case Sequential (l) => Sequential (l.map(transpose (_,n)))
  }

  // mirror de obj au tour du center c
    def mirror (obj:ObjectMusical, c:Int ):ObjectMusical =
  obj match {
    case Note(p,d,v) => Note(c - (p - c),d,v)
    case Rest(d) => Rest(d)
    case Parallel (l) => Parallel (l.map(mirror (_,c)))
    case Sequential (l) => Sequential (l.map(mirror (_,c)))
  }

  // retrograde un obj
    def retrograde (obj:ObjectMusical):ObjectMusical =
  obj match {
    case Sequential (l) => Sequential (l.reverse.map(retrograde))
    case o => o
  }

  //Question 5


  // make a sequential avec n fois obj
  def repeat(obj: ObjectMusical, n: Int): ObjectMusical = {
    Sequential(List.fill(n)(copy(obj)))
  }

  // make obj en parallele avec lui meme avec un decalage de n ms.
  def canon(obj: ObjectMusical, n: Int): ObjectMusical = {
    Parallel(List(obj, shift(obj, n)))
  }


  //  Met obj1 et obj2 en seqeunce
  def concat(obj1: ObjectMusical, obj2: ObjectMusical): ObjectMusical = {
    Sequential(List(obj1, obj2))
  }

  //Question 5 BACH
  val voix1 = Sequential(List(
    Note(60, 750, 106), Note(62, 250, 108), Note(63, 250, 108),
    Note(64, 250, 109), Note(65, 250, 109), Note(66, 250, 110),
    Note(67, 1000, 110), Note(68, 625, 113), Note(65, 125, 114),
    Note(61, 125, 112), Note(60, 125, 112), Note(59, 500, 112),
    Rest(500), Rest(500), Note(67, 1000, 109), Note(66, 1000, 108),
    Note(65, 1000, 106), Note(64, 1000, 106), Note(63, 1000, 106),
    Note(62, 750, 106), Note(61, 250, 106), Note(58, 250, 106),
    Note(57, 250, 106), Note(62, 500, 106), Rest(1000),
    Note(67, 1000, 106), Note(65, 500, 106), Note(64, 1000, 106)))

  val voix2 = Sequential(List(
    Rest(125), Note(48, 125, 100), Note(51, 125, 100),
    Note(55, 125, 100), Note(60, 1000, 100), Note(58, 250, 100),
    Note(57, 250, 100), Note(58, 625, 100), Note(52, 125, 100),
    Note(50, 125, 100), Note(52, 125, 100), Note(53, 125, 100),
    Note(48, 125, 100), Note(53, 125, 100), Note(55, 125, 100),
    Note(56, 750, 100), Note(56, 250, 100), Note(55, 250, 100),
    Note(53, 250, 100), Note(51, 625, 100), Note(51, 125, 100),
    Note(53, 125, 100), Note(51, 125, 100), Note(50, 250, 100),
    Note(48, 250, 100), Note(49, 500, 100), Rest(250), Note(50, 250, 100),
    Note(51, 250, 100), Note(50, 250, 100), Note(48, 125, 100),
    Note(47, 125, 100), Note(48, 125, 100), Note(47, 125, 100),
    Note(48, 125, 100), Note(50, 125, 100), Note(48, 125, 100),
    Note(46, 125, 100), Note(45, 125, 100), Note(43, 125, 100),
    Note(45, 125, 100), Note(46, 125, 100), Note(48, 125, 100),
    Note(45, 125, 100), Note(46, 125, 100), Note(48, 125, 100),
    Note(50, 250, 100), Note(60, 500, 100), Note(58, 125, 100),
    Note(57, 125, 100), Note(58, 250, 100), Note(55, 250, 100),
    Note(52, 250, 100), Note(57, 125, 100), Note(55, 125, 100),
    Note(54, 250, 100), Note(55, 125, 100), Note(57, 125, 100),
    Note(58, 250, 100), Note(49, 250, 100), Note(50, 500, 100),
    Rest(500), Rest(250), Note(50, 375, 100), Note(53, 125, 100),
    Note(52, 125, 100),
    Note(50, 125, 100), Note(49, 125, 100), Note(50, 125, 100),
    Note(52, 125, 100), Note(53, 125, 100), Note(55, 125, 100),
    Note(58, 125, 100), Note(57, 125, 100), Note(55, 125, 100)))


def canon_Bach(): ObjectMusical = {
  // Première voix : thème répété 6 fois avec transposition progressive
  val voix1Canon = repeat(voix1, 6)
  val voix1Transpositions = (0 until 6).foldLeft(voix1Canon: ObjectMusical) { (acc, i) =>
    concat(acc, transpose(voix1, i * 2))
  }

  // Deuxième voix : thème répété 6 fois avec transposition progressive
  val voix2Canon = repeat(voix2, 6)
  val voix2Transpositions = (0 until 6).foldLeft(voix2Canon: ObjectMusical) { (acc, i) =>
    concat(acc, transpose(voix2, i * 2))
  }

  // Troisième voix : deuxième voix transposée d'une quinte (7 demi-tons) et décalée
  val voix3Base = transpose(voix2, 7)
  val voix3Canon = repeat(voix3Base, 6)
  val voix3Transpositions = (0 until 6).foldLeft(voix3Canon: ObjectMusical) { (acc, i) =>
    concat(acc, transpose(voix3Base, i * 2))
  }
  val voix3Decalee = shift(voix3Transpositions, 2000)

  // Résultat final : parallélisme des trois voix
  Parallel(List(voix1Transpositions, voix2Transpositions, voix3Decalee))
}

}
//////////////////////////////////////////////////


/** ************** MAin object ****************** */
object bach extends App {
  val system = ActorSystem("Bach")
  val localActor = system.actorOf(Props[BachActor], name = "Bach")
  localActor ! "START"
}