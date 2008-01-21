#!/bin/sh -e

git-archive --format=tar --prefix=FoX-3.1.2/ HEAD | gzip -9 > ../FoX-3.1.2-devel.tgz

mkdir tmpFoX
cd tmpFoX
tar xzf ../../FoX-3.1.2-devel.tgz
cd FoX-3.1.2
make cutdown
cd ..
tar czf FoX-3.1.2.tgz FoX-3.1.2
rm -rf FoX-3.1.2

for i in wxml wcml sax dom
do
  tar xzf ../../FoX-3.1.2-devel.tgz
  (
    cd FoX-3.1.2
    (
      cd config; \
       sed -e "s/CUTDOWN_TARGET=.*/CUTDOWN_TARGET=$i/" -i "" configure.ac; \
       make
    )
    make cutdown-$i
  )
  tar czf FoX-3.1.2-$i.tgz FoX-3.1.2
  rm -rf FoX-3.1.2
done

cp FoX-3.1.2-* ../..

cd ..
rm -rf tmpFoX

