package model

import akka.actor._
import model.Util.Position
import util.control.Breaks._
import play.api.libs.json._
import model.Entities._
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import model.Util.util.settings

object PlayerActor {
  def props(out: ActorRef, server: ActorRef, userID: Option[String], nick: String) = Props(new PlayerActor(out, server, userID, nick))
}

class PlayerActor(val out: ActorRef, var server: ActorRef, var userID: Option[String], var nick: String) extends Actor with PlayerPacketHandler with ActorLogging {
  //implicit val userFormat = Json.format[PlayerData]
  var name: String = nick
  var roomActor: ActorRef = null;
  var score: Int = 0
  var totalMass: Int = 0
  var isInTheTop = false
  var worldActor: ActorRef = null
  var playerID = Int.MinValue
  var cells = HashSet.empty[Cell]
  var visibleEntities = HashSet.empty[Entity]
  //var worldGrid: WorldGrid = WorldGrid()
  var worldGrid: WorldGrid = null;
  var topLeft = Position(0, 0)
  var bottomRight = Position(0, 0)
  var connected: Boolean = false;
  var rangeX: Double = 0
  var rangeY: Double = 0
  var centerX: Double = 0
  var centerY: Double = 0
  var viewLeft: Double = 0
  var viewRight: Double = 0
  var viewTop: Double = 0
  var viewBottom: Double = 0
  var eatenOwnCells = HashSet.empty[Cell]

  override def postStop() = {
    if (!cells.isEmpty) {
      for (cell <- cells)
        cell.onRemove()
    }

    roomActor ! Leave(userID)
    roomActor ! RemoveFromLeaderBoard(playerID)

    if (score > 0) {
      userID map { uID => roomActor ! SaveMyScore(score, uID) }
      score = 0;
    }
  }

  def getVisibleEntities(): List[Entity] = {
    visibleEntities.toList
  }

  def playerReceive: Receive = {
    case SpawnData(uID: Int, initialPosition: Position, _roomActor: ActorRef, world: WorldGrid, _worldActor: ActorRef) =>
      println("Przyszlo spawn data");
      playerID = uID
      roomActor = _roomActor
      worldGrid = world
      worldActor = _worldActor
      val newCell = Cell(this, initialPosition, worldGrid, _worldActor)
      cells += newCell
      updateView();
      println("Po update view");
      sendMyFristCellID(newCell.id)
      sendNewScore(score)
      connected = true
      println("Spawn player zakonczone");

    case GameTick => {
      if (connected) {
        //println ("przyszlo tick");

        for (cell <- cells)
          cell.move()

        for (cell <- cells)
          cell.checkCollisionWithOwnCells()

        eatenOwnCells.clear()
        for (cell <- cells) {
          cell.eat()
          cell.updatePositionInGrid()
        }

        for (cell <- eatenOwnCells) {
          cells -= cell
          cell.onRemove()
        }

        var allMass = 0

        for (cell <- cells)
          allMass += cell.mass

        log.info("Calkowita masa: " + allMass)
        log.info("Calkowita ilosc: " + cells.size);

        updateNodes()
      }
    }

    case RestartGame(startPosition: Position) =>
      score = 0
      totalMass = 0
      isInTheTop = false
      cells = HashSet.empty[Cell]
      rangeX = 0
      rangeY = 0
      centerX = 0
      centerY = 0
      viewLeft = 0
      viewRight = 0
      viewTop = 0
      viewBottom = 0
      eatenOwnCells = HashSet.empty[Cell]
      val newCell = Cell(this, startPosition, worldGrid, worldActor)
      cells += newCell
      sendMyFristCellID(newCell.id)
      sendNewScore(score)
      updateView()

    case NewLeaderBoard(leaderBoard: List[LeaderBoardEntry]) => {
      if (leaderBoard.length <= 10 || leaderBoard.last.totalMass <= this.getTotalMass(true)) {
        if (!cells.isEmpty)
          sender ! new LeaderBoardEntry(this.playerID, this.name, this.getTotalMass(true))
        //   isInTheTop = true
      }
      //else isInTheTop = false
      println(leaderBoard)
      this.sendLeaderBoard(leaderBoard: List[LeaderBoardEntry])
    }

    case AddMass(mass: Int, eatingEntity: Entity) => {
      println("Przyszlo add mass, mass: " + mass + "entity: " + eatingEntity)
      eatingEntity match {
        case c: Cell =>
          if (cells.contains(c)) {
            log.info("ADD MASS W CELLI")
            c.addMass(mass)
            c.updatePositionInGrid()
            score += mass
            sendNewScore(score)
            log.info("Masa po dodaniu: " + c.mass);
          } else {
            log.info("PRZYSZLO ADD MASS NIEOBSLUZONE ---------------------------")
          }
      }
    }

    case SplitCellByVirusCollision(sourceCell: Cell, angle: Double, mass: Int, speed: Double) => {
      splitCellByVirusCollision(sourceCell, angle, mass, speed)
    }

    case EatCell(c: Cell, eatingEntity: Entity) => {
      log.info("playerID :" + this.playerID + " przyszlo eat cell " + "Cella do zjedzenia: id: " + c.id + " Cela jedzaca: " + eatingEntity.id);

      if (cells.contains(c)) {
        cells -= c
        c.onRemove()
        updateView()
        sender ! AddMass(c.mass, eatingEntity)
        log.info("PRZYSZLO EAT CELL------------------------")
      }

      log.info("Po usunieciu");
      for (ce <- cells)
        log.info("c.id :" + ce.id);

      if (cells.isEmpty) {
        showEndMenu(score)
        if (score > 0) {
          userID map { uID => roomActor ! SaveMyScore(score, uID) }
          score = 0;
        }
      }
    }

    case _ => {
      out ! Json.obj("Hmm" -> "Cos nie tak")
    }
  }

  def receive = playerPacketHandler orElse playerReceive
  override type Receive = PartialFunction[Any, Unit]

  def getTotalMass(reCalcMass: Boolean) = {
    if (reCalcMass) {
      var s = 0;
      for (myCell <- this.cells)
        s += myCell.mass;
      this.totalMass = s;
    }
    this.totalMass;
  };

  def splitCells() = {
    if (cells.size <= settings.playerMaxCells) {
      var newCells = ListBuffer.empty[Cell]
      for (cell <- cells) {
        if (cell.mass > settings.playerMinMassToSplit)
          newCells += cell.split()
      }

      for (newCell <- newCells) {
        cells += newCell
        newCell.updatePositionInGrid()
        sendNewCellId(newCell.id)
      }

      updateView()
      for (tmpCell <- cells)
        println(tmpCell.mass)
    }

    var allMass = 0

    for (cell <- cells)
      allMass += cell.mass

    println("Calkowita masa: " + allMass)
    println("Calkowita ilosc: " + cells.size);
  }

  def ejectMass() = {
    for (cell <- cells)
      if (cell.mass >= settings.minMassEject) {
        cell.ejectMass()
        //worldActor ! EjectedMass(newMass)
      }
  }

  def splitCellByVirusCollision(sourceCell: Cell, angle: Double, mass: Int, speed: Double) = {
    // Starting position
    var startPos = Position(sourceCell.position.x, sourceCell.position.y)
    val newCell = Cell(this, startPos, worldGrid, worldActor)

    newCell.mass = mass
    newCell.color = sourceCell.color
    newCell.moveAngle = angle
    newCell.speed = speed
    newCell.setMoveEngine(15)
    newCell.recombineTicks = settings.playerRecombineTime
    this.cells += newCell
    sendNewCellId(newCell.id)
    updateView()
    //newCell.ignoreCollision = true; // Remove collision checks
  };

  def updateNodes() = {
    updateView();

    var newVisible = worldGrid.getListOfEntities(Position(viewLeft, viewTop), Position(viewRight, viewBottom))

    var removals = HashSet.empty[Entity]
    var updates = HashSet.empty[Entity]
    for (e <- visibleEntities) {
      if (!newVisible.contains(e)) {
        removals.add(e);
      }
    }

    visibleEntities = visibleEntities filterNot (x => removals contains x)

    for (e <- newVisible) {
      if (!visibleEntities.contains(e)) {
        visibleEntities += e
        updates.add(e);
      }
    }

    for (e <- visibleEntities) {

      if (e.shouldUpdate()) {
        updates.add(e);
      }
    }

    sendUpdatesAndRemoves(removals.toList, updates.toList)

  }

  def updateRange() = {
    var totalSize = 1.0D;
    for (cell <- cells)
      totalSize += cell.getPhysicalSize();

    var factor = Math.pow(Math.min(64.0D / totalSize, 1), 0.4D);
    rangeX = settings.view.baseX / factor;
    rangeY = settings.view.baseY / factor;
  }

  def updateCenter(): Unit = {
    if (cells.isEmpty)
      return

    var size = cells.size;
    var x: Double = 0;
    var y: Double = 0;

    for (cell <- cells) {
      x += cell.position.x
      y += cell.position.y
    }

    this.centerX = x / size;
    this.centerY = y / size;
  }

  def updateView() = {
    updateRange();
    updateCenter();

    viewTop = centerY - rangeY;
    viewBottom = centerY + rangeY;
    viewLeft = centerX - rangeX;
    viewRight = centerX + rangeX;

    //lastViewUpdateTick = world.getServer().getTick();
  }
}