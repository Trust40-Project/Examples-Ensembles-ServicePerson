package scenarios

import tcof.{Component, _}


class RunningExample extends Model {

  abstract class Role
  case class HeadOfCompany(company: Company) extends Role
  case class HeadOfDepartment(department: Department) extends Role
  case class Secretary(department: Department) extends Role
  case class Technician(department: Department) extends Role
  case class HeadOfEngineering(process: ManufacturingProcess) extends Role
  case class Serviceman(machine: Machine) extends Role

  class Company(val id: String, val parentCompany: Company = null) extends Component {
    name(s"Company $id")
  }

  class Department(val id: String, val company: Company) extends Component {
    name(s"Department $id (company: ${company.id})")
  }

  class Person(val id: String, val company: Company, val location: String, val roles: Role*) extends Component {
    name(s"Person $id (company: ${company.id})")

    def hasRole(check: PartialFunction[Role, Boolean]): Boolean = {
      roles.exists(role => check.applyOrElse(role, (role: Role) => false))
    }
  }

  class Machine(val id: String, val department: Department) extends Component {
    name(s"Machine $id (department: ${department.id}, company: ${department.company.id})")
  }

  class ManufacturingProcess(val id: String, val start: Long, val end: Long, val machines: Machine*) extends Component {
    name(s"Process $id")
  }


  /*
  (1) different departments within the brakes supplier have a head of department who has got access to the error rates
      of the machines of his/her particular department but not to the error rates of machines in other departments of
      the same factory.
   */
  class SharingErrorRatesWithinSameDepartment(val department: Department) extends Ensemble {
    name(s"SharingErrorRatesWithinSameDepartment for department ${department.name}")

    val machines = role("machines", components.select[Machine].filter(_.department == department))

    val persons = role("persons", components.select[Person].filter(
      _.hasRole{
        case HeadOfDepartment(`department`) => true
      }
    ))

    allow(persons, machines, "errorRates")
  }


  /*
  (2) The head of engineering responsible for particular manufacturing process should have access to error rates, but
      only of the machines involved in the process and only to the error rates time-wise related to the process.
   */
  class SharingErrorRatesWithResponsibleForProcess(val process: ManufacturingProcess) extends Ensemble {
    name(s"SharingErrorRatesWithResponsibleForProceess for process ${process.name}")

    val machines = role("machines", components.select[Machine].filter(process.machines.contains(_)))

    val persons = role("persons", components.select[Person].filter(_.hasRole{
      case HeadOfEngineering(`process`) => true
    }))

    allow(persons, machines, s"errorRates[${process.start}:${process.end}]")
  }


  /*
  (3) A serviceman from a subcontractor may see detailed log rates of a machine to which he/she has been assigned to
      fix, by only if accompanied a technician from the department responsible for the machine.
   */
  class SharingErrorRatesWithServiceman(val machine: Machine) extends Ensemble {
    name(s"SharingErrorRatesWithServiceman for machine ${machine.name}")
    val servicemen = role("servicemen", components.select[Person].filter(
      _.hasRole{
        case Serviceman(`machine`) => true
      }
    ))
    val accompaniment = role("accompaniment", components.select[Person].filter(
      _.hasRole{
        case Technician(machine.department) => true
      }
    ))

    membership(
      servicemen.all(svc => accompaniment.some(acc => svc.location == acc.location))
    )

    allow(servicemen, machine, "errorRates")
  }


  /*
  (4) The brakes supplier wants to share its error rates with a head of a subsidiary company (to allow for quality
      improvement). However, it requires that the data are not shared in the raw form, but only after proper
      anonymization, so as not to reveal details of the manufacturing process.
   */
  class SharingErrorRatesWithSubsidiary(val company: Company) extends Ensemble {
    name(s"SharingErrorRatesWithSubsidiary for company ${company}")
    val machines = role("machines", components.select[Machine].filter(_.department.company == company))
    val persons = role("persons", components.select[Person].filter(
      _.hasRole{
        case HeadOfCompany(subsidiary) => subsidiary.parentCompany == company
      }
    ))

    allow(persons, machines, "sum(errorRates)", PrivacyLevel.RESTRICTED)
  }


  class System extends RootEnsemble {
    val sharingErrorRatesWithinSameDepartment = ensembles(
      components.collect{case department: Department => department}.map(new SharingErrorRatesWithinSameDepartment(_))
    )
    val sharingErrorRatesWithResponsibleForProcess = ensembles(
        components.collect{case process: ManufacturingProcess => process}.map(new SharingErrorRatesWithResponsibleForProcess(_))
    )
    val sharingErrorRatesWithServiceman = ensembles(
      components.collect{case machine: Machine => machine}.map(new SharingErrorRatesWithServiceman(_))
    )
    val sharingErrorRatesWithSubsidiary = ensembles(
      components.collect{case company: Company => company}.map(new SharingErrorRatesWithSubsidiary(_))
    )
  }

  val rootEnsemble = root(new System)
}

object RunningExample {
  def main(args: Array[String]): Unit = {
    val scenario = new RunningExample
    scenario.init()

    val companyA = new scenario.Company("A")
    val departmentAA = new scenario.Department("A", companyA)
    val departmentAB = new scenario.Department("B", companyA)
    val machineAA = new scenario.Machine("A", departmentAA)
    val machineAB = new scenario.Machine("B", departmentAA)
    val processAA = new scenario.ManufacturingProcess("A", 0, 3600, machineAA, machineAB)
    val personAA = new scenario.Person("A", companyA, "location1", new scenario.HeadOfDepartment(departmentAA))
    val personAB = new scenario.Person("B", companyA, "location2", new scenario.Secretary(departmentAA))
    val personAC = new scenario.Person("C", companyA, "location3", new scenario.HeadOfDepartment(departmentAB))
    val personAD = new scenario.Person("D", companyA, "location4", new scenario.Secretary(departmentAB))
    val personAE = new scenario.Person("E", companyA, "location5", new scenario.HeadOfEngineering(processAA))
    val personAF = new scenario.Person("F", companyA, "location1", new scenario.Technician(departmentAA))
    val personAG = new scenario.Person("F", companyA, "location2", new scenario.Technician(departmentAA))
    val personAH = new scenario.Person("H", companyA, "location1", new scenario.Technician(departmentAB))

    val companyB = new scenario.Company("B")
    val departmentBA = new scenario.Department("A", companyB)
    val departmentBB = new scenario.Department("B", companyB)
    val machineBA = new scenario.Machine("A", departmentBB)
    val machineBB = new scenario.Machine("B", departmentBB)
    val personBA = new scenario.Person("A", companyB, "location1", new scenario.HeadOfDepartment(departmentBA))
    val personBB = new scenario.Person("B", companyB, "location2", new scenario.Secretary(departmentBA))
    val personBC = new scenario.Person("C", companyB, "location3", new scenario.HeadOfDepartment(departmentBB))
    val personBD = new scenario.Person("D", companyB, "location4", new scenario.Secretary(departmentBB))

    val companyC = new scenario.Company("C", companyA)
    val departmentCA = new scenario.Department("A", companyC)
    val personCA = new scenario.Person("A", companyC, "location1", new scenario.HeadOfCompany(companyC))

    val companyD = new scenario.Company("D")
    val servicemanDA = new scenario.Person("A", companyD, "location1", new scenario.Serviceman(machineAA))


    scenario.components = List(
      companyA, departmentAA, departmentAB, machineAA, machineAB, processAA, personAA, personAB, personAC, personAD, personAE, personAF, personAG, personAH,
      companyB, departmentBA, departmentBB, machineBA, machineBB, personBA, personBB, personBC, personBD,
      companyC, departmentCA, personCA,
      companyD, servicemanDA
    )

    scenario.rootEnsemble.resolve()

    println(scenario.rootEnsemble.instance.toString)
    println(s"Solution utility: ${scenario.rootEnsemble.instance.solutionUtility}")

    println("Can head of department A in company A see the error rates of machine A in company a: " +
      scenario.rootEnsemble.instance.sharingErrorRatesWithinSameDepartment.selectedMembers.exists(sharing => sharing.persons.selectedMembers.contains(personAA) && sharing.machines.selectedMembers.contains(machineAA)))
  }

}

