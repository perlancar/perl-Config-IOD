package Config::IOD::Constants;

# DATE
# VERSION

BEGIN {
    our %constants = (
        COL_TYPE => 0,

        COL_B_RAW => 1,

        COL_D_COMMENT_CHAR => 1,
        COL_D_WS1 => 2,
        COL_D_WS2 => 3,
        COL_D_DIRECTIVE => 4,
        COL_D_WS3 => 5,
        COL_D_ARGS_RAW => 6,
        COL_D_NL => 7,

        COL_C_WS1 => 1,
        COL_C_COMMENT_CHAR => 2,
        COL_C_COMMENT => 3,
        COL_C_NL => 4,

        COL_S_WS1 => 1,
        COL_S_WS2 => 2,
        COL_S_SECTION => 3,
        COL_S_WS3 => 4,
        COL_S_WS4 => 5,
        COL_S_COMMENT_CHAR => 6,
        COL_S_COMMENT => 7,
        COL_S_NL => 8,

        COL_K_WS1 => 1,
        COL_K_KEY => 2,
        COL_K_WS2 => 3,
        COL_K_WS3 => 4,
        COL_K_VALUE_RAW => 5,
        COL_K_NL => 6,
    );
}

use constant \%constants;

use Exporter qw(import);
our @EXPORT_OK = sort keys %constants;
our %EXPORT_TAGS = (ALL => \@EXPORT_OK);

# ABSTRACT: Constants used when parsing IOD document
