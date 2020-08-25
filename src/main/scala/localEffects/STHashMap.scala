package localEffects

import scala.reflect.ClassTag
import scala.collection.mutable.HashMap

sealed abstract class STHashMap[S,K,V] {
  protected def value:HashMap[K,V]
  def get(k:K):ST[S,Option[V]] = ST(value.get(k))
  def +=(kv:(K,V)):ST[S,Unit] = ST(value+=kv)
  def -=(k:K):ST[S,Unit] = ST(value-=k)
}
object STHashMap {
  def apply[S,K,V]:ST[S,STHashMap[S,K,V]] = ST(new STHashMap[S,K,V] {
    val value = HashMap.empty[K,V]
  })
  def fromMap[S,K,V](m:Map[K,V]):ST[S,STHashMap[S,K,V]] = ST(new STHashMap[S,K,V]{
    val value = (HashMap.newBuilder[K,V]++=m).result()
  })
}
