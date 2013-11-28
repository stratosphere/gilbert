/**
 * gilbert - Distributed Linear Algebra on Sparse Matrices
 * Copyright (C) 2013  Sebastian Schelter
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package nio.ssc.gilbert

object Examples {

  def cooccurrences = {

    val A = load("/home/ssc/Desktop/gilbert/test/matrix.tsv", 3, 3)

    val B = binarize(A)
    val C = B.t * B

    val D = C / C.max()

    WriteMatrix(D)
  }

  def pageRank = {

    val A = load("/home/ssc/Desktop/gilbert/test/matrix.tsv", 3, 3)

    val initialRanks = ones(3, 1) / 3
    val e = ones(3, 1)

    fixpoint(initialRanks, { p => 0.85 * A * p + 0.15 * e })
  }

  def powerIteration = {

    val A = load("/home/ssc/Desktop/gilbert/test/matrix.tsv", 3, 3)

    val bZero = ones(3, 1) / math.sqrt(3)

    fixpoint(bZero, { b => (A * b) / norm2(A * b) })
  }
  
  def kMeans = {

    val A = load("/home/ssc/Desktop/gilbert/test/matrix.tsv", 3, 3)
    val initialCentroids = load("/home/ssc/Desktop/gilbert/test/matrix.tsv", 3, 3)

//    val numCentroids = size(initialCentroids, 1)
//
//    val clusters = fixpoint(initialCentroids, { C => {
//      val D = (sum(A :* A) * ones(1, numCentroids)) - (2 * A * C.t) + (sum(C :* C) * ones(1, numCentroids)
//      val I = D
//
//      (I.t * A) :/ (sum(C.t) *  * ones(1, numCentroids)))
//    }})





  }


//  def linearRegression = {
//
//    val minusOne = scalar(-1)
//
//    val V = load("/home/ssc/Desktop/gilbert/test/matrix.tsv", 3, 3)
//    val y = LoadVector("/y")
//
//    val w = LoadVector("/w")
//    val lambda = scalar(0.000001)
//
//    // r = -V' * y
//    val r = minusOne.times(V.transpose().times(y))
//    // p = -r
//    val p = minusOne.times(r)
//
//    // norm_r2 = sum(r * r)
//    val norm_r2 = r.norm2Squared()
//
//    //TODO start loop
//
//    // q = V' * V * p + lambda * p
//    val q = V.transpose().times(V).times(p).plus(lambda.times(p))
//
//    // alpha = norm_2 / p'*q
//    val alpha = norm_r2.div(p.dot(q))
//
//    // w = w + alpha * p
//    val w_next = w.plus(alpha.times(p))
//
//    // old_norm_r2=norm_r2;
//    //val old_norm_r2 = norm_r2
//
//    // r = r + alpha * q
//    //val r_next = r.plus(alpha.times(q))
//
//    // norm_r2 = sum(r * r)
//    //val norm_r2_next = r_next.norm2Squared()
//
//    // beta=norm_r2/old_norm_r2;
//    //val beta = norm_r2.div(old_norm_r2)
//
//    // p=-r+beta * p;
//    //val p_next = minusOne.times(r.plus(beta.times(p)))
//
//    //TODO end loop
//
//    w_next
//  }

}
