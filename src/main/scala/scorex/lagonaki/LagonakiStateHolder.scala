package scorex.lagonaki

import scorex.NodeStateHolder
import scorex.consensus.{History, StoredBlockchain}
import scorex.perma.consensus.PermaConsensusBlockData
import scorex.serialization.BytesParseable
import scorex.transaction.box.proposition.PublicKey25519Proposition
import scorex.transaction.state.database.LagonakiUnconfirmedTransactionsDatabase
import scorex.transaction.state.{MinimalState, PersistentLagonakiState}
import scorex.transaction.{LagonakiTransaction, MemoryPool, SimplestTransactionalData}

class LagonakiStateHolder(folderOpt: Option[String]) extends NodeStateHolder[PublicKey25519Proposition, LagonakiTransaction, SimplestTransactionalData, PermaConsensusBlockData] {

  override val history = new StoredBlockchain[PublicKey25519Proposition, PermaConsensusBlockData, LagonakiTransaction, SimplestTransactionalData] {
    override val dataFolderOpt: Option[String] = folderOpt

    override val transactionalParser: BytesParseable[SimplestTransactionalData] = SimplestTransactionalData
    override val consensusParser: BytesParseable[PermaConsensusBlockData] = PermaConsensusBlockData
  }
  override val mempool = new LagonakiUnconfirmedTransactionsDatabase(folderOpt)
  override val state = new PersistentLagonakiState(folderOpt)

  override protected val globalState: (MinimalState[PublicKey25519Proposition, LagonakiTransaction], History[PublicKey25519Proposition, LagonakiTransaction, SimplestTransactionalData, PermaConsensusBlockData], MemoryPool[LagonakiTransaction]) =
    (state, history, mempool)

  override def stableState: (MinimalState[PublicKey25519Proposition, LagonakiTransaction], History[PublicKey25519Proposition, LagonakiTransaction, SimplestTransactionalData, PermaConsensusBlockData], MemoryPool[LagonakiTransaction]) =
    globalState

}
