


```sh


./mill clean

./mill __.reformat

./mill regenesca.test

./mill example.run

# for local dev/test
./mill  regenesca.publishLocal


$VERSION="0.1.0"
git commit --allow-empty -am "Release $VERSION"
git tag -a $VERSION -m "Release $VERSION"
git push  --atomic origin main $VERSION


```