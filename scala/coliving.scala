import scala.io.Source

//Define a case class addressData which stores the occupancy data
case class AddressData(
                        customerId: String = "",
                        addressId: String = "",
                        fromDate: Int = 0,
                        toDate: Int = 0
                      )

case class groupStat(
                      addressId: String,
                      fromDate: Int,
                      toDate: Int,
                      individuals: Int
                    ) {
  override def toString: String = {
    "\nGroup with AddressId: " + addressId + ", from: " + fromDate + ", to: " + toDate + " contains: " + individuals + " occupants"
  }
}

object coliving {
  def main(args: Array[String]): Unit = {
    //The full path to the file to import
    val fileName = "data/address_data.csv"
    //The lines of the CSV file (dropping the first to remove the header)
    val addressLines = Source.fromFile(fileName).getLines().drop(1)
    val occupancyData: List[AddressData] = addressLines.map { line =>
      val split = line.split(',')
      AddressData(split(0), split(1), split(2).toInt, split(3).toInt)
    }.toList
    // Sorts ascending
    val sortedOccupancyData = occupancyData.sortBy(r => (r.addressId, r.customerId))

    val groupings = new Groupings(sortedOccupancyData)
    // get the groupings
    val occupantsInGroups = groupings.getGroupsPerAddress()

    // calculate the group's from and to dates, number of members
    val groupsWithDates = occupantsInGroups.map(l => new groupStat(l.head.addressId, l.map(i => i.fromDate).min, l.map(i => i.toDate).max, l.length))

    println("There are " + groupsWithDates.length + " groups")
    println("Following is the list of groups")
    println(groupsWithDates)
  }
}