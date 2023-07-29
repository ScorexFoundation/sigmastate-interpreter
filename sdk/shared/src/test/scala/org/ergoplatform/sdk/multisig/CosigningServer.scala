package org.ergoplatform.sdk.multisig

import scala.collection.mutable

class CosigningServer {
  val sessions: mutable.Map[SessionId, SigningSession] = mutable.Map.empty

  def addSession(session: SigningSession): SessionId = {
    require(!sessions.contains(session.id), s"Session for tx ${session.id} already exists")
    sessions.put(session.id, session)
    session.id
  }

  def getSession(id: SessionId): Option[SigningSession] = {
    sessions.get(id)
  }

  def getSessionsFor(signer: Signer): Seq[SigningSession] = {
    val signerKeys = signer.allKeys.toSet
    sessions.values.filter { s =>
      s.positionsToProve.exists { positionedLeafs =>
        positionedLeafs.exists(pl => signerKeys.contains(pl.leaf))
      }
    }.toSeq
  }
}

object CosigningServer {
}