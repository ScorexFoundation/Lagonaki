package scorex.lagonaki

import java.io.{File, RandomAccessFile}
import java.nio.file.{Paths, Files}

import scorex.api.http.TransactionsApiRoute
import scorex.app.{Application, ApplicationVersion}
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.authds.merkle.versioned.MvStoreVersionedMerklizedIndexedSeq
import scorex.crypto.authds.storage.{KVStorage, MvStoreStorageType}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.network.message.MessageSpec
import scorex.perma.consensus.{PermaAuthData, PermaConsensusBlockData, PermaConsensusModule}
import scorex.perma.settings.{PermaConstants, PermaSettings}
import scorex.perma.storage.AuthDataStorage
import scorex.settings.Settings
import scorex.transaction.box.proposition.PublicKey25519Proposition
import scorex.transaction.{LagonakiTransaction, SimpleTransactionModule, SimplestTransactionalData}
import scorex.utils.ScorexLogging
import shapeless.Sized

import scala.reflect.runtime.universe._

class Lagonaki(settingsFilename: String) extends {
  override protected val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq()
  val applicationName: String = "test"
  val appVersion: ApplicationVersion = ApplicationVersion(0, 0, 0)
} with Application {
  override type CData = PermaConsensusBlockData
  override type P = PublicKey25519Proposition
  override type TX = LagonakiTransaction
  override type TData = SimplestTransactionalData

  override implicit val settings = new Settings with PermaSettings {
    override lazy val filename = settingsFilename

    override lazy val rootHash: Array[Byte] = Base58.decode("88YUWmUu3VvwaWwZyoJAjoXyLCN6K3aZBJPKtp9VFi3D").get
  }

  implicit lazy val authDataStorage: KVStorage[Long, PermaAuthData, MvStoreStorageType] =
    new AuthDataStorage(Some(settings.authDataStorage))

  val rootHash = settings.rootHash

  if (settings.isTrustedDealer) dealerSetup()

  override implicit val transactionModule = new SimpleTransactionModule(settings, networkController)
  val consensusModule = new PermaConsensusModule(Sized.wrap(rootHash), settings, transactionModule)

  override val apiRoutes = Seq(TransactionsApiRoute(transactionModule, settings))
  override val apiTypes = Seq(typeOf[TransactionsApiRoute])

  private def dealerSetup(): Unit = {
    val TreeFileName = MvStoreVersionedMerklizedIndexedSeq.TreeFileName
    val SegmentsFileName = MvStoreVersionedMerklizedIndexedSeq.SegmentsFileName
    val tree = if (Files.exists(Paths.get(settings.treeDir + TreeFileName + "-0.mapDB"))) {
      log.info("Get existing tree")
      MvStoreVersionedMerklizedIndexedSeq(
        Some(settings.treeDir + TreeFileName),
        Some(settings.treeDir + SegmentsFileName),
        1,
        FastCryptographicHash)
    } else {
      log.info(s"Generating random data set of size ${PermaConstants.n * PermaConstants.segmentSize}")
      val treeDir = new File(settings.treeDir)
      treeDir.mkdirs()
      val datasetFile = settings.treeDir + "/data.file"
      new RandomAccessFile(datasetFile, "rw").setLength(PermaConstants.n * PermaConstants.segmentSize)
      log.info("Calculate tree")
      val tree = MvStoreVersionedMerklizedIndexedSeq.fromFile(
        datasetFile, Some(settings.treeDir), PermaConstants.segmentSize, FastCryptographicHash)

      def addBlock(i: Long): Unit = {
        val p = tree.elementAndProof(i).get
        authDataStorage.set(i, new PermaAuthData(p.data, p.proof))
        if (i > 0) {
          addBlock(i - 1)
        }
      }

      log.info("Test tree")
      val index = PermaConstants.n - 3
      val leaf = tree.elementAndProof(index).get
      require(leaf.check(tree.rootHash)(FastCryptographicHash))

      log.info("Put ALL data to local storage")
      new File(settings.treeDir).mkdirs()

      addBlock(PermaConstants.n - 1)
      tree
    }
    require(tree.rootHash sameElements rootHash, s"${Base58.encode(tree.rootHash)} != ${Base58.encode(rootHash)}")
  }
}


object Lagonaki extends App with ScorexLogging {

  log.debug("Start server with args: {} ", args)
  val filename = args.headOption.getOrElse("settings.json")

  val application = new Lagonaki(filename)

  log.debug("PermaScorex has been started")
  application.run()

  if (application.wallet.privateKeyAccounts().isEmpty) application.wallet.generateNewAccounts(1)
  //
  //  if (application.settings.testScript) testingScript()
  //
  //  def testingScript(): Unit = {
  //    log.info("Going to execute testing scenario")
  //    log.info("Current state is:" + application.blockStorage.state)
  //    val wallet = application.wallet
  //
  //    if (wallet.privateKeyAccounts().isEmpty) {
  //      wallet.generateNewAccounts(3)
  //      log.info("Generated Accounts:\n" + wallet.privateKeyAccounts().toList.map(_.address).mkString("\n"))
  //    }
  //
  //    log.info("Executing testing scenario with accounts" +
  //      s"(${wallet.privateKeyAccounts().size}) : "
  //      + wallet.privateKeyAccounts().mkString(" "))
  //
  //    require(wallet.privateKeyAccounts().nonEmpty)
  //
  //    Thread.sleep(3.seconds.toMillis)
  //
  //    val genesisBlock = application.blockStorage.history.genesis
  //
  //    def genPayment(recipient: Option[Account] = None, amtOpt: Option[Long] = None): Option[Transaction] = {
  //      val pkAccs = wallet.privateKeyAccounts().ensuring(_.nonEmpty)
  //      val senderAcc = pkAccs(Random.nextInt(pkAccs.size))
  //      val senderBalance = application.blockStorage.state.asInstanceOf[BalanceSheet].balance(senderAcc.address)
  //      val recipientAcc = recipient.getOrElse(pkAccs(Random.nextInt(pkAccs.size)))
  //      val fee = Random.nextInt(5).toLong + 1
  //      if (senderBalance - fee > 0) {
  //        val amt = amtOpt.getOrElse(Math.abs(Random.nextLong() % (senderBalance - fee)))
  //        Some(application.transactionModule.createPayment(senderAcc, recipientAcc, amt, fee))
  //      } else None
  //    }
  //
  //    log.info("Generate 200 transactions")
  //    (1 to 200) foreach (_ => genPayment())
  //
  //    (1 to Int.MaxValue).foreach { _ =>
  //      Thread.sleep(Random.nextInt(5.seconds.toMillis.toInt))
  //      log.info(s"Payment created: ${genPayment()}")
  //    }
  //  }
}
