def chooseFastDataStructuresForAdt(
  adt: AbstractDataType): DominanceFrontier[CompositeDataStructure] = {
  val options: Set[CompositeDataStructure] =
    allDataStructures.subsets().map((subset) => findAllTimes(subset, adt))

  DominanceFrontier.fromSet(options)
}



def getAllTimes(set of implementations): MethodTimes

def getAllTimesForDataStructures(dataStructures: Set[DataStructure]): MethodTimes = {
  val allReadTimes = dataStructures.flatMap(_.readImpls) ++ defaultReadImpls

  val writeTimesForDataStructures = dataStructures.flatMap((ds) => {
    getAllTimes(ds.writeImpls ++ )
  })

  val overallWriteTimes = writeTimesForDataStructures.values.reduce(_ + _)

  allReadTimes ++ overallWriteTimes
}
