


```sh


./mill clean

./mill __.reformat

./mill regenesca.test

./mill example.run

# for local dev/test
./mill  regenesca.publishLocal



# RELEASE
$VERSION="0.6.1"
git commit --allow-empty -am "Release $VERSION"
git tag -a $VERSION -m "Release $VERSION"
git push --atomic origin main --tags


```