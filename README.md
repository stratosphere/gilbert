gilbert
=======

In order to compile MRuntime one has to build manually spark with Scala 2.10

1. Clone current Scala 2.10 branch
git clone https://github.com/apache/incubator-spark.git -b scala-2.10

2. Build packages
sbt/sbt packaging

3. Install compiled packages in local repository
mvn install

4. Enjoy
