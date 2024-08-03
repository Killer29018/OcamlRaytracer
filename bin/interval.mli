module Interval : sig
    type interval_T = {
        mutable min: float;
        mutable max: float
    }

    val empty : interval_T
    val universe : interval_T

    val create : float -> float -> interval_T

    val zero_infinite : interval_T

    val unit : interval_T

    val size : interval_T -> float

    val expand : interval_T -> interval_T -> interval_T

    val pad : interval_T -> float -> interval_T

    val contains : interval_T -> float -> bool

    val clamp : interval_T -> float -> float

    val to_string : interval_T -> string
end
