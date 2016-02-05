package model

import akka.actor.ActorRef
import model.Util.Position
import model.Entities._
import java.awt.Color
import model.Entities.EntityType._
import java.util.UUID
import model.Util.util.RoomDescription

sealed abstract class ServerMessage
case class GetUniqueId ()  extends ServerMessage
case class Join (userID: Option[String])  extends ServerMessage
//case class TickMove ()  extends ServerMessage
case class GameTick ()  extends ServerMessage
//case class SubscribedID (uID: Int) extends ServerMessage
//case class Goodbye(uID: Int) extends ServerMessage
//case class PlayerData(id: Int, x: Double, y: Double) extends ServerMessage
//case class EntitiesInView(ent: List[PlayerData]) extends ServerMessage
case class SpawnData(uID: Int, initialPosition: Position, roomActor: ActorRef, worldGrid: WorldGrid, worldActor: ActorRef) extends ServerMessage
case class GiveMeUniqueId() extends ServerMessage
//case class UpdateData(id: Int, x: Double, y: Double, size: Double, R: Int, G: Int, B: Int, isSpiked: Boolean) extends ServerMessage
case class UpdateData(id: Int, x: Double, y: Double, size: Double, R: Int, G: Int, B: Int, eType: Int, name: String) extends ServerMessage
case class RemoveData(id: Int)
case class EntitiesInView(ent: List[Entity]) extends ServerMessage
//case class Eat(id: Int) extends ServerMessage
case class EatCell(c: Cell, eatingEntity: Entity) extends ServerMessage
case class EatFood(f: Food) extends ServerMessage
case class PlayerCellsData(l: List[Cell]) extends ServerMessage
//case class EjectedMass(m: Mass) extends ServerMessage
case class EjectedMass(startPosition: Position, angle: Double, color: Color) extends ServerMessage
case class EatEntity(m: Entity, eatingEntity: Entity) extends ServerMessage
case class PlayerData(id: Int, x: Double, y: Double, size: Double, R: Int, G: Int, B: Int) extends ServerMessage
case class AddMass(mass: Int, eatingEntity: Entity) extends ServerMessage
case class FeedVirus(v: Virus, m: Mass) extends ServerMessage
case class RemoveNode(e: Entity) extends ServerMessage
case class ShootVirus(sourceVirus: Virus) extends ServerMessage
case class SplitCellByVirusCollision(sourceCell: Cell, angle: Double, mass: Int, speed: Double) extends ServerMessage
case class LeaderBoardUpdateTick(players: List[ActorRef]) extends ServerMessage
case class RemoveFromLeaderBoard(playerID: Int) extends ServerMessage
case class NewLeaderBoard(leaderBoard: List[LeaderBoardEntry]) extends ServerMessage
case class LeaderBoardEntryPacket(id: Int, name: String) extends ServerMessage
case class JoinRoom(idRoom: Int, userID: Option[String]) extends ServerMessage
case class GiveServer(idRoom: Int) extends ServerMessage
case class GiveRooms() extends ServerMessage
case class RoomPacket(id: Int, title: String, lat: Double, lng: Double) extends ServerMessage
case class FriendPacket(friendID: String, roomID: String, name: String, avatar: String) extends ServerMessage
case class JoinChatMap(userID: Option[UUID]) extends ServerMessage
case class AddNewRoom(title: String, lat: Double, lng: Double) extends ServerMessage




case class AddFacebookFriend(myFacebookID: String, friendFacebookID: String) extends ServerMessage
case class NotifyFriendAboutMyNewRoom(userID: UUID, roomID: Option[Int]) extends ServerMessage
case class LeaveChatMap(userID: Option[UUID]) extends ServerMessage
case class FriendChangedRoomPacket(friendID: UUID, roomID: Int) extends ServerMessage
case class GetUsersRooms(users: List[UUID]) extends ServerMessage
case class AddedNewServerRoom(roomID: Int, roomDsc: RoomDescription) extends ServerMessage

//SERVER MESSAGES
case class AddNewServerRoom(roomID: Int) extends ServerMessage
case class AddNewServerRoomResponse(roomID: Option[Int]) extends ServerMessage
case class Leave(userID: Option[String]) extends ServerMessage
case class UserJoinedGame(userID: String, roomID: Int) extends ServerMessage
case class UserLeftGame(userID: String) extends ServerMessage
case class RestartMyGame() extends ServerMessage
case class RestartGame(startPosition: Position) extends ServerMessage
case class SaveMyScore(score: Int, userID: String) extends ServerMessage
case class LoggedUserInfo(name: String, avatar: String, score: Int) extends ServerMessage


case class NodeActivated(address: String) extends ServerMessage
case class NodeActivatedConfirmation() extends ServerMessage

