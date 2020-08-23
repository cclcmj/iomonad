/*
 * @Author: mj
 * @Date: 2020-08-23 05:24:59
 * @LastEditTime: 2020-08-23 05:52:14
 * @LastEditors: Please set LastEditors
 * @Description: In User Settings Edit
 * @FilePath: /iomonad/src/main/scala/localEffects/LocalEffects.scala
 */
package localEffects

object LocalEffects {
    //纯函数式的可变状态，可变的数组qicksort
    def quicksort(xs:List[Int]):List[Int] = 
        if(xs.isEmpty) xs else {
            val arr = xs.toArray
            //交换数组中的两个元素
            def swap(x:Int,y:Int) = {
                val tmp = arr(x)
                arr(x) = arr(y)
                arr(y) = tmp
            }
            //n到r中，将数组的元素与pivot比较，比它大的和比它小的各分一部分
            def partition(n:Int,r:Int,pivot:Int) = {
                val pivotVal = arr(pivot)
                swap(pivot,r)
                var j = n
                for (i <- n until r) if(arr(i) < pivotVal) {
                    swap(i,j)
                    j += 1
                }
                swap(j,r)
                j
            }
            //基于元数组的分治排序
            def qs(n:Int,r:Int):Unit = if(n<r) {
                val pi = partition(n,r,n+(r-n)/2)
                qs(n,pi-1)
                qs(pi+1,r)
            }
            qs(0,arr.length-1)
            arr.toList
        }
    
}
