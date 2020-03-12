#!/usr/bin/env node
'use strict';
const Main = require('./output/index.js');

const file = process.argv[2] 
console.log(file)
Main.main(file)()
