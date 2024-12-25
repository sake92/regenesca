


```sh


./mill clean

./mill __.reformat

./mill regenesca.test

./mill example.run

# for local dev/test
./mill  regenesca.publishLocal


# RELEASE
# bump regenescaVersion to x.y.z !!!
$VERSION="0.4.1"
git commit --allow-empty -am "Release $VERSION"
git tag -a $VERSION -m "Release $VERSION"
git push --atomic origin main --tags


# prepare for NEXT version
# bump regenescaVersion to x.y.z-SNAPSHOT
$VERSION="0.4.2-SNAPSHOT"
git commit -am"Bump version to $VERSION"
git push

```