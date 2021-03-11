s11 () {
    stack --resolver="lts-11" "$@"
}

s12 () {
    stack --resolver="lts-12" "$@"
}

s14 () {
    stack --resolver="lts-14" "$@"
}

make-self-tar () {
    vanilla=$(find ./dist-newstyle -name vanilla -print -quit)
    dest=test/data/self

    echo "=== preparing ==="
    rm -vrf $dest
    mkdir -vp $dest

    echo "=== coying files ==="
    cp -vr $vanilla/mix $vanilla/tix $dest

    echo "=== removing Paths_hpc_codecov.mix ==="
    find $dest -name 'Paths_hpc_codecov.mix' -exec rm {} +

    echo "=== creating tar ==="
    tar -v -C test/data -c -f $dest.tar self

    echo "=== cleaning up ==="
    rm -vrf $dest
}

make-reciprocal-tar () {
    tmpdir=$(mktemp -d)
    echo Made $tmpdir
    mkdir -p $tmpdir/reciprocal
    cp reciprocal/reciprocal.hs $tmpdir/reciprocal
    ( cd $tmpdir/reciprocal && ghc -fhpc -fforce-recomp reciprocal.hs )
    ( cd $tmpdir/reciprocal && echo 7 | ./reciprocal )
    rm -vf $tmpdir/reciprocal/reciprocal{,.o,.hi}
    tar -v -C $tmpdir -c -f $tmpdir/reciprocal.tar reciprocal
    mv $tmpdir/reciprocal.tar .
    rm -rvf $tmpdir
}

hpc-codecov-cabal () {
    cabal v2-exec -- hpc-codecov "$@"
}

hpc-codecov-stack () {
    stack exec -- hpc-codecov "$@"
}

report-cabal-v2 () {
    version=$(hpc-codecov-cabal --numeric-version)
    proj=hpc-codecov-$version
    vanilla=$(find ./dist-newstyle -name vanilla -print -quit)
    mix=$vanilla/mix/$proj
    tix=$(find ./dist-newstyle -name $proj.tix)
    hpc-codecov-cabal --verbose -m $mix $tix
}
