#! /bin/sh

DIR=$(pwd)
PATH_TO_ME=$(dirname $0)
cd $PATH_TO_ME

SCRATCH="../../build/scratch/"$$
mkdir $SCRATCH

cp -R bin/* $SCRATCH

for j in $(find lib -type f -name "*.jar")
do
    echo $j
    cp -R $j $SCRATCH
    cd $SCRATCH
    jar xf $(basename $j)
    rm $(basename $j)
    cd $DIR
done

cd $SCRATCH
rm -rf META-INF
jar cf feedshub_harness.jar $(find . -name '*.class' -type f)
cd $DIR

cp $SCRATCH/feedshub_harness.jar .
zip -Tv feedshub_harness.jar

rm -rf $SCRATCH
cd $DIR

echo Jar created in $PATH_TO_ME/feedshub_harness.jar
