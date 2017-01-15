package etape1
import org.jacop.scala._

object TimeTableProjet extends jacop {
  
  // String Lists with day name.
    val days = List("Monday", "Tuesday", "Wednesday", "Thursday", "Friday");

    // String Lists with hour name.
    val hours = List("8:30", "9:30", "10:30", "11:30", "13:00", "14:00", "15:00", "16:00");

    // String Lists with prof name.
    val teachers = List("Seront", "Grolaux", "Yakoub", "Collinet", "Belina", "margos");
    val iSeront = 0; val iGrolaux = 1; val iYakoub = 2; val iCollinet = 3; val iBelina = 4; val iMargos = 5;

    // String Lists with serie name.
    val series = List("Série 1", "Série 2");
    val iSerie1 = 0; val iSerie2 = 1;
    
    // String Lists with room name.
    val rooms = List("017", "019", "B23", "Aud A", "Aud B", "B11", "B12", "B21", "025", "026", "B25", "D3");
    val i017 = 0; val i019 = 1; val iB23 = 2; val iA = 3; val iB = 4; val iB11 = 5; val iB12 = 6; val iB21 = 7; val i025 = 8; val i026 = 9; val iB25 = 10; val iD3 = 11;

    // String Lists with course name.
    // A empty course for a empty timeslot
    val courses = List("algo", "apoo", "truc", "anglais", "");
    val iCoursVide = 4; val iAlgo = 0; val iApoo = 1; val iTruc = 2; val iAnglais = 3;

    // FUNCTIONS

    // returns (day,hour) from an index 
    def getDayAndHour(indice: Int): (Int, Int) = {
      ((indice - (indice % hours.length)) / hours.length, indice % hours.length);
    };
    // return the index from a day and a hour
    def getIndice(day: Int, hour: Int): Int = {
      day * (hours.length) + hour
    };

    // une Map qui contient pour chaque série son horaire complet 
    // pour chaque plage horaire elle contient un cours, un local et un professeur
    val HoraireSeries = {
      for {
        uneSerie <- List.range(0, series.length)
      } yield (uneSerie) -> {
        for (i <- List.range(0, getIndice(days.length - 1, hours.length)))
          yield (List(IntVar("CourseName", 0, courses.length - 1), IntVar("Room", 0, rooms.length - 1), IntVar("ProfessorName", 0, teachers.length - 1)));
      }
    }.toMap;
	// Map qui pour chaque cours de chaque série stocke une liste de flags
    // Chaque indice de la liste correspond à une plage horaire.
    // Si le flag est mis à vrai, alors on sait que le cours a lieu pour la plage horaire qu'il représente
    val ContraintesHoraire = {
      for {
        course <- List.range(0, courses.length)
        serie <- List.range(0, series.length)
      } yield (course, serie) -> {
        for (indice <- List.range(0, getIndice(days.length, hours.length)))
          yield BoolVar(courses(course) + " - " + series(serie) + " " + indice)
      }
    }.toMap;
    
// Map qui pour chaque prof dans chaque série stocke une liste de flags
    // Chaque indice de la liste correspond à une plage horaire.
    // Si le flag est mis à vrai, alors on sait que le prof donne cours pour la plage horaire qu'il représente

    val ContraintesProf = {
      for {
       prof <- List.range(0, teachers.length)
       serie <- List.range(0, series.length)
      } yield (prof, serie) -> {
        for (indice <- List.range(0, getIndice(days.length, hours.length) ))
          yield BoolVar(teachers(prof) + " - " + series(serie) + " " + indice)
      }
    }.toMap;
    
  
  def main(args: Array[String]) = {
    //[CONSTRAINTS FUNCTIONS]
    def comptageDesHeuresPourUnProf(indice: Int) = {

      for (uneSerie <- List.range(0, HoraireSeries.size)) {

        for (unProf <- List.range(0, teachers.length)) {

          //  S'il s'agit d'une heure dédiée pour ce cours , le flag sera à true
          ContraintesProf(unProf, uneSerie)(indice) <=> (HoraireSeries(uneSerie)(indice)(2) #= unProf);
        }

      }
    }
    
    def comptageDesHeuresPourUnCours(indice: Int) = {

      for (uneSerie <- List.range(0, HoraireSeries.size)) {

        for (unCours <- List.range(0, courses.length)) {

          //  S'il s'agit d'une heure dédiée pour ce cours , le flag sera à true
          ContraintesHoraire(unCours, uneSerie)(indice) <=> (HoraireSeries(uneSerie)(indice)(0) #= unCours);
        }

      }
    }


    def nePeutPasDonnerCoursEnMemeTemps(indice: Int) = {
      for (uneSerie <- List.range(0, HoraireSeries.size - 1)) {
        for (uneAutreSerie <- List.range(uneSerie + 1, HoraireSeries.size)) {

          // un prof ne peut pas donner deux cours ou plus en meme temps
    // « indice » correspond a une plage horaire et le « 2 » correspond aux professeurs 	
          HoraireSeries(uneSerie)(indice)(2) #\= HoraireSeries(uneAutreSerie)(indice)(2);

          // deux cours ou plus ne peuvent pas être donnés dans le même local en meme temps
	    // « indice » correspond a une plage horaire et le « 1 » correspond aux locaux 	
          HoraireSeries(uneSerie)(indice)(1) #\= HoraireSeries(uneAutreSerie)(indice)(1);

        }
      }
    }

    def neVeutPasDonnerCoursAvantUneCertaineHeure(indice: Int, uneSerie: Int, heure: Int, prof: Int) = {

      // si le moment de la journée (indice) correspond à/aux heure(s) prohibés par le prof 
      // List.range pour simplifier les tests du modulo
      // « indice » correspond a une plage horaire et le « 2 » correspond aux professeurs 	
      if ((indice % hours.length) < heure) {
        HoraireSeries(uneSerie)(indice)(2) #\= prof;

      }
    }

    def veutTravaillerUnJourPrecisAPartirDUneCertaineHeure(indice: Int, uneSerie: Int, jour: Int, heure: Int, prof: Int) = {

      val dayHour = getDayAndHour(indice);

      if (dayHour._1 != jour || dayHour._2 < heure) {
        HoraireSeries(uneSerie)(indice)(2) #\= prof;
      }

    }

    def nePeutPasDonnerUnCours(indice: Int, uneSerie: Int, cours: Int, prof: Int) = {

      // applique la contrainte d'impossibilité de donner ce cours par ce prof
      NOT(AND(HoraireSeries(uneSerie)(indice)(2) #= prof, HoraireSeries(uneSerie)(indice)(0) #= cours));

    }

    def definirLeNombreDHeureDeCoursParSemaine(uneSerie: Int, cours: Int, nombre: Int) = {

      // Contraindre que le calcul ( la somme tous les flags à true) soit égale au nombre choisi

      sum(ContraintesHoraire(cours, uneSerie)) #= nombre;

    }

    // certains cours pourraient exiger d'avoir un certain local à un certain moment de l'horaire
    // Par exemple : Pour la série 1, le cours Apoo a lieu le lundi et le mercredi à 8h30 et exige uniquement le local 017

    // if ( indice == getIndice(0, 0) || indice == getIndice(2,0) ) {
    //    exigerAccesExclusifAUnLocal( indice ,iApoo, 0, i017);
    //  }

    def exigerAccesExclusifAUnLocal(indice: Int, cours: Int, serie: Int, local: Int) = {
      // garantir l'unicité du local pour le cours
      AND(
        HoraireSeries(serie)(indice)(1) #= local,
        HoraireSeries(serie)(indice)(0) #= cours);
    }

    /*
     * le prof fini sa journée avant une certaine heure
     * 
     * @param heure : heure à la quelle le prof veut finir sa journée ! 
     * 								exemple: si il veut finir a 14h il donnera son dernier cours a 13h
     * 
     * */ 
    def finirAvantHeure(indice: Int, uneSerie: Int, heure: Int, prof: Int) = {

       if ((indice % hours.length) > heure-1) {
        HoraireSeries(uneSerie)(indice)(2) #\= prof;

      }
    }
    
    //le prof a droit a un certain jour d'absence 
    def definirJourDeRepos(indice: Int, uneSerie: Int, jour: Int, prof: Int) = {

      
      val dayHour = getDayAndHour(indice);

      if (dayHour._1 == jour) {
        HoraireSeries(uneSerie)(indice)(2) #\= prof;
      }
      
    }
    
    // Un prof donne un nombre d’heures de cours par semaine défini par (param)
    def definirNombreDeCoursParProf(uneSerie: Int, prof: Int, nombre: Int) = {

      // Contraindre que le calcul ( la somme tous les flags à true) soit égale au nombre choisi
      
      sum(ContraintesProf(prof, uneSerie)) #= nombre;     

    }
    
    // Un prof donne maximum x heures de cours par semaine
    def definirNombreDeCoursMaximalParProf(uneSerie: Int, prof: Int, nombre: Int) = {

      // Contraindre que le calcul ( la somme tous les flags à true) soit égale au nombre choisi
      
      sum(ContraintesProf(prof, uneSerie)) #<= nombre;     

    }
    

    //[CONSTRAINTS] 

    for (indice <- List.range(0, getIndice(days.length - 1, hours.length))) {

      // CONTRAINTE N°1 :
      // un prof ne peut pas donner deux cours ou plus en meme temps
      // deux cours ne peuvent pas être donnés dans le même local
      // « indice » correspond a une plage horaire
      nePeutPasDonnerCoursEnMemeTemps(indice);
      
      // Contraintes Horaire : Placer le flag à true d'un cours pour une série si celui ci est donné au temps X
      // « indice » correspond a une plage horaire
      comptageDesHeuresPourUnCours(indice);
      
      // Contraintes Prof : Placer le flag à true si le cours est donné par un certain prof
      // « indice » correspond a une plage horaire
      comptageDesHeuresPourUnProf(indice);
      
      // CONTRAINTE N°2 :
      //le cours d'anglais a les contraintes suivantes :
      // -> se donne seulement au B23
      // -> a lieu le Lundi ou le Mercredi à 8h30
      // « indice » correspond a une plage horaire
      if (indice == getIndice(0, 0) || indice == getIndice(2, 0)) {
        exigerAccesExclusifAUnLocal(indice, iAnglais, 0, iB23)
      }
      
      // CONTRAINTE N°3 :
      // Mr Margos donne cours que le lundi a partir de 14h
      // « indice » correspond a une plage horaire, « 0 » est la serie 1, « 0 » correspond a lundi et « 5 » correspond a 14h00
      veutTravaillerUnJourPrecisAPartirDUneCertaineHeure(indice, 0, 0, 5, iMargos);

      for (uneSerie <- List.range(0, HoraireSeries.size)) {

        // CONTRAINTE N°4 :
        // Un prof ne veut pas donner cours avant XX H
        
        neVeutPasDonnerCoursAvantUneCertaineHeure(indice, uneSerie, hours indexOf ("10:30"), iGrolaux);
                
        // CONTRAINTE N°5
        // Un prof veut finir avant une certaine heure
        finirAvantHeure(indice, uneSerie, hours indexOf ("14:00"), iSeront);
        
        // CONTRAINTE N°6 :
        // Un prof souhaite travailler un seul jour (Mercredi) à partir de 10h30
        veutTravaillerUnJourPrecisAPartirDUneCertaineHeure(indice, uneSerie, 2, 2, iYakoub);
        
        // CONTRAINTE N°7 :
        // Mr Collinet a son jour de repos le mardi
        definirJourDeRepos(indice, uneSerie, 1, iCollinet);
      }

      // CONTRAINTE N°8 :
      // chaque prof a des incapacités à donner certains cours pour tous les series

      for (uneSerie <- List.range(0, HoraireSeries.size)) {
                
        // Mme iBelina ne peut donner aucun cours, excepté Anglais
        for (unCours <- List.range(0, courses.length) if (unCours != iAnglais) ) {
          nePeutPasDonnerUnCours(indice, uneSerie, unCours, iBelina);
        }
        
        // Tous les autres prof ne peuvent pas donner Anglais
        for (unProf <- List.range(0, teachers.length) if unProf != iBelina ) {
          nePeutPasDonnerUnCours(indice, uneSerie, iAnglais, unProf);
        }
        
        nePeutPasDonnerUnCours(indice, uneSerie, iTruc, iCollinet);
        nePeutPasDonnerUnCours(indice, uneSerie, iApoo, iSeront);
        nePeutPasDonnerUnCours(indice, uneSerie, iAlgo, iGrolaux);

      }

      // ou : pour une série en particulier (ici la série 1)
      nePeutPasDonnerUnCours(indice, iSerie1 , iApoo, iYakoub);

    }

    // CONTRAINTE N°9 :
    // Chaque cours a un nombre d'heure précis :
    // Ici , choix arbitraire que tous les séries ont le même nombre
    for (uneSerie <- List.range(0, HoraireSeries.size)) {
      definirLeNombreDHeureDeCoursParSemaine(uneSerie, iAlgo, 8);
      definirLeNombreDHeureDeCoursParSemaine(uneSerie, iTruc, 10);
      definirLeNombreDHeureDeCoursParSemaine(uneSerie, iAnglais, 2);
      definirLeNombreDHeureDeCoursParSemaine(uneSerie, iApoo, 8);
      
      
      // CONTRAINTE N°10
      // Definir le nombre d'heure par prof
      // Ici , Mr Grolaux donne 8h de cours par série par semaine
      definirNombreDeCoursParProf(uneSerie, iGrolaux, 8);
      
      // CONTRAINTE N°11
      // Definir le nombre d'heure maximal par prof
      // Ici, Mr Margos donne au maximum 8h de cours par série par semaine
      definirNombreDeCoursMaximalParProf(uneSerie, iMargos, 8);
    }

    // prints Solution
    // Version tableau
    def printSol(): Unit = {

      for (serie <- List.range(0, HoraireSeries.size)) {
        println("");
        println("Horaire " + series(serie));
        print("\t");
        for (day <- List.range(0, days.length)) {

          print(days(day) + "             \t");

        }
        println();

        for (hour <- List.range(0, hours.length)) {
          print(hours(hour) + "  ");
          for (day <- List.range(0, days.length)) {
            val result =
              if (HoraireSeries(serie)(getIndice(day, hour))(0).value == iCoursVide)
                "Pas de cours\t"
              else
                courses(HoraireSeries(serie)(getIndice(day, hour))(0).value) + " " + rooms(HoraireSeries(serie)(getIndice(day, hour))(1).value) + " " + teachers(HoraireSeries(serie)(getIndice(day, hour))(2).value);
            print(result + "   \t");

          }
          println("\n");

        }

      }

    }

    // prints Solution
    // Version Liste
    def printSol2(): Unit = {

      for (serie <- List.range(0, HoraireSeries.size)) {
        println("");
        println("Horaire " + series(serie));

        for (day <- List.range(0, days.length)) {

          println("\t" + days(day));

          for (hour <- List.range(0, hours.length)) {

            // List( IntVar("CourseName", 1, 3) , IntVar("Room", 1, 2) , IntVar("ProfessorName", 1, 4)

            val result =
              if (HoraireSeries(serie)(getIndice(day, hour))(0).value == iCoursVide)
                hours(hour) + " : Pas de cours"
              else
                hours(hour) + " : " + courses(HoraireSeries(serie)(getIndice(day, hour))(0).value) + " " + rooms(HoraireSeries(serie)(getIndice(day, hour))(1).value) + " " + teachers(HoraireSeries(serie)(getIndice(day, hour))(2).value);
            println("\t\t " + result);
          }

        }

      }

    }

    println("LANCEMENT DE LA RECHERCHE");
    // ContraintesHoraire.values.flatten.toList ::

    // Now time to prepare JaCop Solver vars
    val intVarsList = HoraireSeries.values.flatten.flatten.toList;
    val boolVarsListHoraire = ContraintesHoraire.values.flatten.toList;
    val boolVarsListProf = ContraintesProf.values.flatten.toList;
    val boolVarsList = boolVarsListHoraire ::: boolVarsListProf;
    //val boolVarsList = boolVarsListHoraire;
    
    val vars = boolVarsList ::: intVarsList;

    val result = satisfy(search(vars, most_constrained, indomain_min), printSol);
    println(result);
  }

}