#! /bin/sh
set -e
IFS='
'
DIR="$(pwd)"
PATH_TO_ME="$(dirname -- "$0")"
cd -- "$PATH_TO_ME"
SCRATCH="../../build/scratch/"$$
mkdir -- "$SCRATCH"

cp -R -- bin/* "$SCRATCH"

for j in $(find lib -type f -name "*.jar")
do
    echo "$j"
    cp -R -- "$j" "$SCRATCH"
    cd -- "$SCRATCH"
    jar xf "$(basename $j)"
    rm -- $(basename $j)
    cd -- "$DIR"
done

cd -- "$SCRATCH"
rm -rf META-INF
touch .make-jar-happy; jar cf feedshub_harness.jar .make-jar-happy
find . -name '*.class' -type f -print0 | xargs -0 jar uf feedshub_harness.jar
cd -- "$DIR"

cp -- "$SCRATCH/feedshub_harness.jar" .
zip -Tv feedshub_harness.jar

rm -rf -- "$SCRATCH"
cd -- "$DIR"

echo "Jar created in $PATH_TO_ME/feedshub_harness.jar"
