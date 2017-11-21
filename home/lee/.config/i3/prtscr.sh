#!/bin/bash
exec import $1 $2 /bigD/images/$(date +%Y%m%d-%H-%M-%S-%N).jpg
