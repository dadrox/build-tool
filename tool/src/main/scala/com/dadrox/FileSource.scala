package com.dadrox

import java.io.File
import java.io.FileNotFoundException

trait FileSource {
    def openFile(path: String): Response[File]
}

class FileProvider extends FileSource {
    override def openFile(path: String): Response[File] = new File(path) match {
        case nonexistant if (!nonexistant.exists) => Fails(Failure.NotFound, "Unable to find file " + path)
        case notFile if (!notFile.isFile)         => Fails(Failure.NotFound, path + " appears to be a directory")
        case noRead if (!noRead.canRead)          => Fails(Failure.NotFound, "Unable to read file " + path + ", please check its permissions")
        case file                                 => Succeeds(file)
    }
}