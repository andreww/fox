#!/bin/sh -e

git-archive --format=tar --prefix=FoX-3.1.2/ HEAD | gzip -9 > ../FoX-3.1.2-full.tar.gz

mkdir tmpFoX
cd tmpFoX
tar xzf ../../FoX-3.1.2-full.tar.gz
(
  cd FoX-3.1.2
  make cutdown
)
tar czf ../../FoX-3.1.2.tar.gz FoX-3.1.2
rm -rf FoX-3.1.2

for i in wxml wcml sax dom
do
  tar xzf ../../FoX-3.1.2-full.tar.gz
  (
    cd FoX-3.1.2
    (
      cd config; \
       sed -e "s/CUTDOWN_TARGET=.*/CUTDOWN_TARGET=$i/" -i "" configure.ac; \
       make
    )
    make cutdown-$i
  )
  tar czf FoX-3.1.2-$i.tar.gz FoX-3.1.2
  rm -rf FoX-3.1.2
done

cp FoX-3.1.2-* ../..

cd ..
rm -rf tmpFoX

