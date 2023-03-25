#!/bin/bash 

# Run the hardware encoder and decoder simulations

NUM_TESTS=10
RS="opam exec -- dune exec bin/rs.exe --"

function simulate_encoder {
    echo "Encoder simulation tests ..."

    $RS simulate encoder -n 15 -t 1 -b 0 -num-tests $NUM_TESTS -seed 123
    $RS simulate encoder -n 15 -t 2 -b 1 -num-tests $NUM_TESTS -seed 123

    $RS simulate encoder -n 31 -t 3 -b 0 -num-tests $NUM_TESTS -seed 123
    $RS simulate encoder -n 31 -t 4 -b 1 -num-tests $NUM_TESTS -seed 123

    $RS simulate encoder -n 127 -t 16 -b 7 -num-tests $NUM_TESTS -seed 123
    $RS simulate encoder -n 127 -t 23 -b 8 -num-tests $NUM_TESTS -seed 123

    $RS simulate encoder -n 255 -t 16 -b 3 -num-tests $NUM_TESTS -seed 123
    $RS simulate encoder -n 255 -t 32 -b 9 -num-tests $NUM_TESTS -seed 123


}

function simulate_decoder {
    PARALLELISM=$1
    echo "Decoder simulation tests, parallelsim $PARALLELISM ..."

    $RS simulate decoder -n 15 -t 1 -b 0 -num-tests $NUM_TESTS -parallelism $PARALLELISM -seed 123
    $RS simulate decoder -n 15 -t 2 -b 1 -num-tests $NUM_TESTS -parallelism $PARALLELISM -seed 123

    $RS simulate decoder -n 31 -t 3 -b 0 -num-tests $NUM_TESTS -parallelism $PARALLELISM -seed 123
    $RS simulate decoder -n 31 -t 4 -b 1 -num-tests $NUM_TESTS -parallelism $PARALLELISM -seed 123

    $RS simulate decoder -n 127 -t 16 -b 7 -num-tests $NUM_TESTS -parallelism $PARALLELISM -seed 123
    $RS simulate decoder -n 127 -t 23 -b 8 -num-tests $NUM_TESTS -parallelism $PARALLELISM -seed 123

    $RS simulate decoder -n 255 -t 16 -b 3 -num-tests $NUM_TESTS -parallelism $PARALLELISM -seed 123
    $RS simulate decoder -n 255 -t 32 -b 9 -num-tests $NUM_TESTS -parallelism $PARALLELISM -seed 123
}

simulate_encoder
simulate_decoder 1
simulate_decoder 2
simulate_decoder 3
simulate_decoder 7
simulate_decoder 8