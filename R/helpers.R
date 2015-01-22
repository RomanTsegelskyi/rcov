#' Reassing object in the namespace
#'
#' Record that particual line was executed. Used in statement coverage, needed for namespace replacement
#' @param name name of an object to be replaced
#' @param obj object that will be put in the environment
#' @param env environment to be replaced in
reassignInEnv <- function(name, obj, env) {
    if (exists(name, env)) {
        if (bindingIsLocked(name, env)) {
            unlockBinding(name, env)
            assign(name, obj, envir = env)
            lockBinding(name, env)
        } else {
            assign(name, obj, envir = env)
        }
    }
}