strv_union <- function(strv1, strv2) {
	union_strv <- strv1

	for(str_el in strv2) {
		if(str_el %!sin% strv1) {
			union_strv <- c(union_strv, str_el)
		}
	}

	union_strv
}

strv_intersect <- function(strv1, strv2) {
	intersect_strv <- c()

	for(str_el in strv1) {
		if(str_el %sin% strv2) {
			intersect_strv <- c(intersect_strv, str_el)
		}
	}

	intersect_strv
}

strv_setdiff <- function(strv1, strv2) {
	setdiff_strv <- character(0)

	for(str_el in strv1) {
		if(str_el %!sin% strv2) {
			setdiff_strv <- c(setdiff_strv, str_el)
		}
	}

	setdiff_strv
}

strv_setequal <- function(strv1, strv2) {
	if(length(strv1) != length(strv2)) {
		return(FALSE)
	}
	for(str_el in strv1) {
		if(str_el %!sin% strv2) {
			return(FALSE)
		}
	}
	TRUE
}

strv_is.element <- function(str_el, strv) {
	if(!requireNamespace("stringi", quietly = TRUE)) {
		stop("Package \"stringi\" needed for this function to work. Please install it.")
	}

	is_el <- logical(0)

	for(el in str_el) {
		is_el <- c(is_el, any(stringi::stri_cmp_equiv(el, strv)))
	}

	is_el
}

`%sin%` <- function(str_el, strv) strv_is.element(str_el, strv)

`%!sin%` <- function(str_el, strv) !strv_is.element(str_el, strv)
