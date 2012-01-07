LOBOUND=1
HIBOUND=2
RANDMAX=32767
BINUMBER=$(( $LOBOUND + ($HIBOUND * $RANDOM) / ($RANDMAX + 1) ))

if [ "$BINUMBER" = "1" ]; then
    echo "Now Zeus was a womanizer, always on the make,"
    echo "but Hera usually punished her that Zeus was wont to take. -- Cake"
fi

if [ "$BINUMBER" = "2" ]; then
    echo "I'm hungry. Let's get a taco. -- Reservoir Dogs"
fi


