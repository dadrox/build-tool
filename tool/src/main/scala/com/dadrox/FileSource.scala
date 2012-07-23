package com.dadrox

import java.io.File
import java.io.FileNotFoundException
import java.io.InputStream
import java.io.FileInputStream
import java.io.InputStreamReader

trait FileSource {
    def asInputStream(path: String): Response[InputStream]
}

class FileProvider extends FileSource {
    override def asInputStream(path: String): Response[InputStream] = {
        for {
            file <- new File(path) match {
                case nonexistant if (!nonexistant.exists) => Fails(Failure.NotFound, "Unable to find file " + path)
                case notFile if (!notFile.isFile)         => Fails(Failure.NotFound, path + " appears to be a directory")
                case noRead if (!noRead.canRead)          => Fails(Failure.NotFound, "Unable to read file " + path + ", please check its permissions")
                case file                                 => Succeeds(file)
            }
            inputStream <- Succeeds(new FileInputStream(file))
        } yield inputStream
    }
}