import scala.annotation.tailrec


/** Class encapsulating methods that take list of individuals and puts them in groups that lived 'together'
  *
  * @param addressList
  */
class Groupings(addressList: List[AddressData]) {

  /** Function outputs lowest fromdate and highest todate of the input list. The input list is expected to be
    * individuals living together with overlapping stays
    *
    * @param x : input list
    * @return
    */
  private def occupiedAddrFromTo(x: List[AddressData]): (Int, Int) = {
    (x.map(i => i.fromDate).min, x.map(i => i.toDate).max)
  }


  /** Function expects list of addresses sorted by fromDate. Output are two lists.
    * Second one holds one co-living group. It holds all the individuals living at one address with overlapping stays.
    * First one holds the rest of the individuals from the input
    *
    * @param toExamine    : List that we want to
    * @param liveTogether : Nil when called
    * @return Two lists.
    */
  @tailrec
  private def intersectLivingTogether(toExamine: List[AddressData], liveTogether: List[AddressData] = Nil): (List[AddressData], List[AddressData]) = {
    (toExamine, liveTogether) match {
      case (Nil, y) => (Nil, y) // We've checked that all the occupants live together, we return it as such
      case (x, Nil) => intersectLivingTogether(x.tail, x.head :: Nil) // We start with putting the first head of the assorted list into the liveTogether
      case (x, y) => {
        // we check whether the head of the toExamine list lived together at the same address as and with some time intersect as the others in liveTogether
        if (x.head.fromDate < occupiedAddrFromTo(y)._2 && x.head.addressId == y.head.addressId) {
          // interval of x.head intersects with y, so we add the x.head to the list that holds these
          intersectLivingTogether(x.tail, y ::: (x.head :: Nil))
        } else {
          // interval of x.head does not intersect, so we return y as those that live together
          (x, y)
        }
      }
    }
  }


  /** getGroups takes list of addressData, outputs list of the co-living groups
    *
    * @param addressList : addressData list - sorted by fromdate
    * @param result      : Nil
    * @return List of the co-living groups
    */
  @tailrec
  private def getGroups(addressList: List[AddressData], result: List[List[AddressData]] = Nil): List[List[AddressData]] = {
    intersectLivingTogether(addressList) match {
      case (Nil, groupsLiveTogether) => result :+ groupsLiveTogether
      case (assorted, groupsLiveTogether) => getGroups(assorted, result :+ groupsLiveTogether)
    }
  }

  /** getGroupsPerAddress takes addressList, outputs list of the co-living groups
    *
    * @return
    */
  def getGroupsPerAddress(): List[List[AddressData]] = {
    val sortedAddressList = addressList.sortBy(x => (x.addressId, x.fromDate))
    getGroups(sortedAddressList)
  }
}
