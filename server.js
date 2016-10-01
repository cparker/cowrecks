'use strict'

const express = require('express')
const morgan = require('morgan')

let app = express()
app.use(morgan('combined'))
app.use(express.static('.'))
app.listen(8000, () => {
  console.log('listening on port 8000')
})
