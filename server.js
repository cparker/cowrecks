'use strict'

const express = require('express')
const morgan = require('morgan')

let app = express()
let port = process.env.PORT || 8080

app.use(morgan('combined'))
app.use(express.static('.'))
app.listen(port, () => {
    console.log(`listening on port ${port}`)
})
