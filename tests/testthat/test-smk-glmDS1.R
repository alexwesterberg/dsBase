#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

context("glmDS1::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

context("glmDS1::smk")
test_that("simple glmDS1, gaussian", {
    # formula, family, weights, offset, data
    formula <- x~y
    family <- "gaussian"
    weights<- NULL
    offset <- NULL
    data <- data.frame(x = c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4), y = c(4.0, 0.0, 3.0, 1.0, 2.0, 2.0, 1.0, 3.0, 0.0, 4.0))
    
    res <- glmDS1(formula, family, weights, offset, "data")

    print(res)
    expect_equal(class(res), "list")
    expect_length(res, 8)
    
    expect_length(res$dimX, 2)
    expect_equal(res$dimX[1], 10)
    expect_equal(res$dimX[2], 2)
    
    expect_length(res$coef.names, 2)
    expect_equal(res$coef.names[1], "(Intercept)")
    expect_equal(res$coef.names[2], "y")
    
    expect_length(res$y.invalid, 1)
    expect_equal(res$y.invalid[1], 0)
    
    expect_length(res$Xpar.invalid, 2)
    expect_equal(res$Xpar.invalid[1], 0)
    expect_equal(res$Xpar.invalid[2], 0) 
    
    expect_length(res$w.invalid, 1)
    expect_equal(res$w.invalid[1], 0)
    
    expect_length(res$o.invalid, 1)
    expect_equal(res$o.invalid[1], 0)
    
    expect_length(res$glm.saturation.invalid, 1)
    expect_equal(res$glm.saturation.invalid[1], 0)
    
    expect_length(res$errorMessage, 1)
    expect_equal(res$errorMessage[1], "No errors")
})

#
# Done
#

context("glmDS1::smk::shutdown")

context("glmDS1::smk::done")
