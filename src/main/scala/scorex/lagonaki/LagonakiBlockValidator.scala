package scorex.lagonaki

import scorex.block.BlockValidator
import scorex.perma.consensus.{PermaConsensusBlockData, PermaValidator}
import scorex.transaction.box.proposition.PublicKey25519Proposition
import scorex.transaction.{LagonakiTransaction, SimpleTransactionValidator, SimplestTransactionalData}

class LagonakiBlockValidator(rootHash: Array[Byte]) extends
BlockValidator[PublicKey25519Proposition, LagonakiTransaction, SimplestTransactionalData, PermaConsensusBlockData](
  SimpleTransactionValidator, new PermaValidator(rootHash))

