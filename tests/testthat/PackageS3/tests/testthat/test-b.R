context('basic.coverage')

test_that('test', {
    testS3(numeric())
    testS3(logical())
})

