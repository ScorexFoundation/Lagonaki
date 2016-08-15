package scorex.lagonaki

import scorex.block.{Block, StateChanges, StateChangesCalculator}
import scorex.perma.consensus.PermaConsensusBlockData
import scorex.transaction.account.PublicKey25519NoncedBox
import scorex.transaction.box.proposition.PublicKey25519Proposition
import scorex.transaction.state.{MinimalState, PersistentLagonakiState}
import scorex.transaction.{LagonakiTransaction, SimplestTransactionalData}

object LagonakiRewardCalculator extends StateChangesCalculator[PublicKey25519Proposition, LagonakiTransaction,
  SimplestTransactionalData, PermaConsensusBlockData] {
  val ProducerReward = 1000000L

  override def changes(block: Block[PublicKey25519Proposition, SimplestTransactionalData, PermaConsensusBlockData], state: MinimalState[PublicKey25519Proposition, LagonakiTransaction]): StateChanges[PublicKey25519Proposition] = {
    val lagonakiState = state.asInstanceOf[PersistentLagonakiState]
    val producer = block.consensusData.producer
    val minerReward: StateChanges[PublicKey25519Proposition] = {
      val reward = ProducerReward + block.transactionalData.transactions.map(_.fee).sum
      val oldRcvrOpt = lagonakiState.accountBox(producer)
      val newRcvr = oldRcvrOpt.map(o => o.copy(value = o.value + reward)).getOrElse {
        PublicKey25519NoncedBox(producer, reward)
      }
      StateChanges[PublicKey25519Proposition](oldRcvrOpt.toSet, Set(newRcvr))
    }
    block.transactionalData.transactions.foldLeft(minerReward) { (changes, tx) =>
      val txChanges = tx.changes(state).get
      require(changes.toRemove.intersect(txChanges.toRemove).isEmpty, "1 tx per block for 1 account allowed for now")
      changes.copy(toAppend = txChanges.toAppend ++ changes.toAppend, toRemove = txChanges.toRemove ++ changes.toRemove)
    }
  }

}
