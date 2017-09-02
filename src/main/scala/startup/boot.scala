package startup



import java.io.{File, PrintWriter}

import scala.io.Source


/** Singleton Application Object for Obtain the SLCSP associated to a ZipCode . Entry Point for the applicaiton
  *
  *  @author angel Figueroa Cruz
  *  @version 0.0.1
  *  @constructor
  *
  *  email angel.figueroa.cruz@gmail.com
  */
object boot extends App {

   System.out.println("Homework Calculate second lowest cost silver plan (SLCSP) Date : Aug 29 2017 ")

   // Constant Value
   val metalSilver = "Silver"
   val cBlank      = " ";

   // Map for Hold the keys
   var mapZIPS  : Map[String,ZIPS] = Map()
   var mapPlans : Map[(String,String),PLANS] = Map()


   // Load all records from the Zips file
   // zipcode,state,county_code,name,rate_area
   val zips  = Source.fromInputStream(getClass.getResourceAsStream("/zips.csv")).getLines().foreach(rec => split (rec,ZIPS))

   // Load all records from the plans file
   // plan_id,state,metal_level,rate,rate_area
   val plans = Source.fromInputStream(getClass.getResourceAsStream("/plans.csv")).getLines().foreach(rec => split (rec,PLANS))


   // File for Output
   val fileName = "slcsp_out.csv"
   val slcsp_out = new PrintWriter(new File(fileName))


   // Load all records from the slcsp file
   // zipcode,rate
   val slcsp = Source.fromInputStream(getClass.getResourceAsStream("/slcsp.csv")).getLines().foreach(rec => split (rec,SLCSP))


   // Close the Output
   slcsp_out.close()

  System.out.println("End Processing.... ")


  /**  Method for Handling the Split the different record types
    *
    *  @param data file record
    *  @param recType type of transaction
    */
  def split (data : String , recType : AnyRef) : Unit = {

    val dta = data.split(",").map(_.trim)

    recType match {

      case ZIPS  => if (dta.length == 5) mapZIPS  += (dta(0) -> ZIPS (dta(0),dta(1), dta(2) , dta (3) , dta(4)))

      case PLANS => if (dta.length == 5) {
        val key = (dta(1), dta(2))
        if (! mapPlans.contains(key)) mapPlans += (key -> PLANS (dta(0),dta(1), dta(2) , dta (3) , dta(4)))
        else mapPlans -= key
      }

      case SLCSP => {

        if (dta.length == 1) {
          val dtaSLCSP = SLCSP (dta(0),"")

          // Write to the File
          slcsp_out.write(s"${dtaSLCSP.zipcode},${getZips (dtaSLCSP.zipcode)} \n")
        }
      }
    }

  }


   /**  Method for Handling the Search on the ZIPS Container
     *
     *  @param key zipcode value
     */
   def getZips (key : String) : String = {

     mapZIPS.contains(key) match {
            case true  => {
                val plan = getPlan((mapZIPS (key).state,metalSilver))
                if (plan.isDefined) plan.get.rate  else cBlank
            }
            case false => cBlank
         }
   }

  /**  Method for Handling the Search for the Plan
    *
    *  @param key (State,Metal Code) Tuple
    */
  def getPlan (key : (String,String)) : Option [PLANS] = {
    mapPlans.contains(key) match {
      case true  => Some(mapPlans (key))
      case false => None
    }
  }

}

// All the class that represent , each file record type
case class ZIPS  (val zipcode : String  ,val state : String,val county_code : String,val name : String,val rate_area : String)
case class SLCSP (val zipcode : String  ,val rate  : String)
case class PLANS (val plan_id : String  ,val state : String ,val metal_level : String ,val rate : String ,val rate_area : String)
