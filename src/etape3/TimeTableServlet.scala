package etape3

import javax.servlet.http.HttpServlet
import org.jacop.scala._

object TimeTableServlet extends HttpServlet {

  import javax.servlet.http.HttpServletRequest
  import javax.servlet.http.HttpServletResponse

  override def doPost(request: HttpServletRequest, response: HttpServletResponse) {
    response.setCharacterEncoding("UTF-8")

    val responseBody = getContents();
    response.setStatus(200);
    response.getWriter().print(responseBody);
  }

  // Créer un JSON de TimeTableProjet
  def getContents(): String = {

    // obtenir données - Tuple qui contient :
    // (result,days,hours,courses,teachers,series,HoraireSeries,rooms,iCoursVide)
    val data = TimeTableProjet.main()
    
    // obtenir les jours et les heures pour les horaires
    val scheduleDays = data._2.mkString("\"days\": [\"", "\",\"", "\"]")
    val scheduleHours = data._3.mkString("\"hours\": [\"", "\",\"", "\"]")
    
    
    // partie qui génère les cours pour une série pour une heure précise
    def JsonSerieFunction3(x : List[String], serie : Int, hour : Int, accum : String) : String = {
      
      x match {
        case Nil => accum
        case head :: tail if tail.length > 0 => if ( data._7(serie)( TimeTableProjet.getIndice(data._2 indexOf(head), hour)  )(0).value == data._9 ) JsonSerieFunction3(tail,serie,hour,accum +"{},")
          else JsonSerieFunction3(tail,serie,hour,accum + "{ \"courseName\" : \""+ data._4(data._7(serie)( TimeTableProjet.getIndice(data._2 indexOf(head), hour)  )(0).value) +
          "\" , \"teacherName\" : \"" + data._5(data._7(serie)( TimeTableProjet.getIndice(data._2 indexOf(head), hour)  )(2).value) +
          "\", \"room\" : \"" + data._8(data._7(serie)( TimeTableProjet.getIndice(data._2 indexOf(head), hour)  )(1).value) + "\"},")
        case head :: tail  => if ( data._7(serie)( TimeTableProjet.getIndice(data._2 indexOf(head), hour)  )(0).value == data._9 ) JsonSerieFunction3(tail,serie,hour,accum +"{}")
          else JsonSerieFunction3(tail,serie,hour,accum + "{ \"courseName\" : \""+ data._4(data._7(serie)( TimeTableProjet.getIndice(data._2 indexOf(head), hour)  )(0).value) +
          "\" , \"teacherName\" : \"" + data._5(data._7(serie)( TimeTableProjet.getIndice(data._2 indexOf(head), hour)  )(2).value) +
          "\", \"room\" : \"" + data._8(data._7(serie)( TimeTableProjet.getIndice(data._2 indexOf(head), hour)  )(1).value) + "\"}")  
      }
      
    }
    
    // Partie du JSON qui contient chaque heure de chaque série avec leur cours de la semaine
    def JsonSerieFunction2(x : List[String], serie : Int , accum : String ) : String = {
      
      x match {
        case Nil => accum
        case head :: tail if tail.length > 0 => JsonSerieFunction2(tail,serie, accum + "\"" + head + "\" : [" + JsonSerieFunction3(data._2,serie,data._3 indexOf(head) , "") + "],")
        case head :: tail => JsonSerieFunction2(tail,serie, accum + "\"" + head + "\" : [" + JsonSerieFunction3(data._2,serie,data._3 indexOf(head) , "") + "]") 
      }
      
    }
    
    def JsonSerieFunction1(x: List[String] , accum : String ) : String = {
      
      x match {
        case Nil => accum
        case head :: tail if tail.length > 0 => JsonSerieFunction1(tail, accum + "\"" + head + "\" : {"+ JsonSerieFunction2(data._3,data._6 indexOf(head), "") + "}," ) 
        case head :: tail => JsonSerieFunction1(tail, accum + "\"" + head + "\" : {"+ JsonSerieFunction2(data._3,data._6 indexOf(head), "") + "}" )
      }
      
    }

    
    println(  "{" + scheduleDays + " , " + scheduleHours + ", \"series\" : {" + JsonSerieFunction1(data._6, "")  + "} }" )
    
    // Seriliaze to JSON string
    "{" + scheduleDays + " , " + scheduleHours + ", \"series\" : {" + JsonSerieFunction1(data._6, "")  + "} }"
  }
  

}