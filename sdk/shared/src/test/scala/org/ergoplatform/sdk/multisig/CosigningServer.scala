package org.ergoplatform.sdk.multisig

import sigmastate.SigmaLeaf

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

  def getSessionsFor(signerKeys: Seq[SigmaLeaf]): Seq[SigningSession] = {
    val keyset = signerKeys.toSet
    sessions.values.filter { s =>
      s.positionsToProve.exists { positionedLeafs =>
        positionedLeafs.exists(pl => keyset.contains(pl.leaf))
      }
    }.toSeq
  }

  def updateSession(session: SigningSession): Unit = {
    sessions.put(session.id, session)
  }
}

object CosigningServer {
}