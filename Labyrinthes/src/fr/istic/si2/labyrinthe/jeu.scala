package fr.istic.si2.labyrinthe
import scala.util.Random
import LabyrintheObject._
import CelluleObject._
import Utils._

object Jeu {

  /**
   * Chemin parcouru lors d'une déambulation dans un labyrinthe
   *
   * @param chemin Le chemin déjà parcouru dans la labyrinthe, position courante incluse.
   *               Il n'est jamais vide (on commence toujours à l'entrée du labyrinthe).
   *               Le chemin est sauvegardé dans l'ordre inverse du parcours :
   *                 - premier élément de la liste : position courante
   *                 - dernier élément de la liste : entrée du labyrinthe
   */
  type Chemin = List[Position]

  /**
   * @param laby un labyrinthe
   * @param chemin un chemin dans laby, sans cycle, c'est à dire ne passant jamais
   *        deux fois sur une même case.
   * @return vrai si le chemin mène à la sortie de laby, faux sinon.
   */
  def estResolu(laby: Labyrinthe, chemin: Chemin): Boolean = {
   def Resolu(chemin:Chemin): Boolean={
    chemin match{
     case Nil => false
     case s::res => (s==sortieLabyrinthe(laby))||Resolu(res)
    }
   }
   Resolu(chemin)
  }

  /**
   * @param laby
   * @return le chemin initial, c'est à dire celui avec lequel on commence à jouer.
   */
  def cheminInitial(laby: Labyrinthe): Chemin = entreeLabyrinthe(laby)::Nil
   
      // TODO
    
  

  /**
   * @param chemin
   * @return le chemin correspondant à l'annulation du dernier déplacement dans chemin,
   *         si des déplacements ont déjà été effectués.
   */
  def annulerBouger(chemin: Chemin): Chemin = {
    chemin match{
      case Nil => Nil
      case s::r::res => val l = annulerBouger(res)
      s::l// TODO
    }
  }

  /**
   *  Direction des déplacements dans un labyrinthe
   *  Utile pour simplifier quelques fonctions
   *
   */
  sealed trait Direction
  case object Haut extends Direction
  case object Bas extends Direction
  case object Droite extends Direction
  case object Gauche extends Direction

  /**
   * @param p une position
   * @param d une direction
   * @return la position voisine de pos, suivant direction
   */
  def voisine(p: Position, d: Direction): Position = {
    (p,d) match{
      case ((x,y),Haut) => (x-1,y)
      case ((x,y),Bas) => (x+1,y)
      case ((x,y),Droite) => (x,y+1)
      case ((x,y),Gauche) => (x,y-1)// TODO
    }
  }

  val directions: List[Direction] = List(Gauche, Bas, Droite, Haut)

  /**
   * @param laby un labyrinthe
   * @param p une position dans laby
   * @return une liste de positions voisines accesssibles depuis p dans laby
   */
  def voisines(laby: Labyrinthe, p: Position): List[Position] = {
    def possible(c:Cellule): Position={
      val (x,y)=p
      (p,c) match{
        case ((x,y),Cellule(Ouvert,_)) => (x-1,y)
        case ((x,y),Cellule(_,Ouvert)) => (x,y+1)
        case ((x,y),Cellule(Ouvert,Ouvert)) => (x-1,y);(x,y+1)
      }
      // TODO
    }
    voisines(laby,p)
  }

  /**
   * @param laby un labyrinthe
   * @param p une position à l'intérieur du labyrinthe
   * @param d une direction
   * @return vrai s'il y a un passage depuis la position p
   *         dans la direction d, et faux sinon.
   *
   * @note   Souvenez-vous qu'un agencement de labyrinthe ne
   *         place pas nécessairement de murs sur les bords
   *         du labyrinthe, mais que les bords d'un
   *         labyrinthe ne peuvent pas être franchis.
   *         Par exemple, passageOuvert(laby,(0,0), Haut)
   *         doit être faux, même si la cellule en (0,0) n'a
   *         pas de mur en haut.
   *
   *         Indication de longueur : moins de 10 lignes
   */
  def passageOuvert(laby: Labyrinthe, p: Position, d: Direction): Boolean = {
    def passOuvert(c:Cellule,p:Position): Boolean ={
     val (x,y) = p
     if(c==Cellule(Ouvert,Ferme)&&d==Haut) true
     else if(c==Cellule(Ferme,Ouvert)&&d==Droite) true
     else false // TODO
    }
    passageOuvert(laby,p,d)
  }

  /**
   * @param e L'état de l'interface graphique
   * @param direction Une direction
   * @return L'état après un pas dans la direction direction, si possible
   */
  def bouge(laby: Labyrinthe, chemin: Chemin, direction: Direction): Chemin = {
    
     def bou(p:Position): Chemin={
      chemin match{
        case Nil => Nil
        case s::res => if(passageOuvert(laby,p,direction)) p::s::res else chemin 
      }
     }
      // TODO
    bouge(laby,chemin,direction)
  }

  /**
   * @laby un labyrinthe
   * @chemin un chemin dans le labyrinthe laby
   * @return une fonction décrivant l'état de chaque cellule du labyrinthe
   *         laby
   *
   * @note cf. cellule.scala, definition de EtatCellule
   */
  def etatLabyrinthe(laby: Labyrinthe, chemin: Chemin): EtatLabyrinthe = {
    def etat(p:Position,d:Direction): EtatCellule={
    if(passageOuvert(laby,p,d)==true) Visitee
    else if(passageOuvert(laby,p,d)==false) NonVisitee
    else Courante
    }// TODO
    etatLabyrinthe(laby,chemin)
  }

  /* Résolution */

  /**
   * @param laby un labyrinthe
   * @param depart une position valide dans le labyrinthe
   * @param arrivee un position valide dans le labyrinthe
   * @return un chemin de depart à arrivee sans boucles, ou, si cela
   *         n'est pas possible, le chemin List(depart)
   *
   * @note Suggestions :
   *
   *       - définir une fonction récursive auxiliaire etendre : Chemin => Chemin qui
   *       étend un chemin donné à un chemin allant jusqu'à arrivee (si possible)
   *       et l'appliquer à List(depart).
   *
   *       - pour définir etendre(chemin1), utiliser filter et map pour exprimer l'idée suivante :
   *       une extension de chemin1 jusqu'à arrivée est obtenue en étendant chemin1 d'une position
   *       (pour chacune des voisines de la tête de chemin1) et en appliquant récursivement resoudre
   *       à ces chemins augmentés. Parmi les extensions possibles obtenues, on garde la première qui
   *       va effectivement jusqu'à arrivee, s'il y en a, ou sinon on retourne chemin1.
   *
   *       - indication de longueur : moins de 10 lignes
   *
   */
  def resoudreDeA(laby: Labyrinthe, depart: Position, arrivee: Position): Chemin = ??? // TODO

  /**
   * @param laby un labyrinthe
   * @param chemin un chemin valide dans le labyrinthe laby
   * @return si cela est possible, un chemin étendant le chemin donné,
   *         menant à la sortie de laby, sans passer deux fois par la
   *         même case (sauf s'il faut rebrousser le chemin
   *         donné).
   *         Si cela n'est pas possible, le chemin donné argument
   *         est renvoyé.
   *
   * @note utiliser resoudreDeA.
   *       Indication de longueur : 1 ou 2  lignes
   *
   */
  def resoudre(laby: Labyrinthe, chemin: Chemin): Chemin = ??? // TODO

  /**
   * @param laby un labyrinthe
   * @param chemin un chemin valide dans le labyrinthe laby
   * @return Si cela est possible, un chemin étendant le chemin donné
   *         d'une seule case le long d'un chemin solution.
   *         Si cela n'est pas possible, le chemin donné est renvoyé.
   *
   * @note utiliser la fonction resoudreDeA
   *
   *       Indication de longueur : 1 ou 2  lignes
   *
   *       La fonction takeRight : List[T] => Int => List[T]
   *       peut être utile. xs.takeRight(n) renvoie les n derniers
   *       éléments de la liste xs.
   */
  def indice(laby: Labyrinthe, chemin: Chemin): Chemin = ??? // TODO

  

}