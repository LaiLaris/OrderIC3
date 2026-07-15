#!/usr/bin/env python3
import pathlib
import sys
import time
import uno
from com.sun.star.beans import PropertyValue


def property_value(name, value):
    prop = PropertyValue()
    prop.Name = name
    prop.Value = value
    return prop


def connect():
    local_ctx = uno.getComponentContext()
    resolver = local_ctx.ServiceManager.createInstanceWithContext(
        "com.sun.star.bridge.UnoUrlResolver", local_ctx
    )
    for _ in range(50):
        try:
            return resolver.resolve(
                "uno:socket,host=localhost,port=2002;urp;StarOffice.ComponentContext"
            )
        except Exception:
            time.sleep(0.1)
    raise RuntimeError("无法连接 LibreOffice")


def main():
    if len(sys.argv) != 2:
        raise SystemExit("用法: inspect_word_doc.py FILE.doc")

    ctx = connect()
    desktop = ctx.ServiceManager.createInstanceWithContext(
        "com.sun.star.frame.Desktop", ctx
    )
    url = uno.systemPathToFileUrl(str(pathlib.Path(sys.argv[1]).resolve()))
    doc = desktop.loadComponentFromURL(
        url, "_blank", 0, (property_value("Hidden", True),)
    )
    try:
        for index, line in enumerate(doc.Text.String.splitlines(), 1):
            if line.strip():
                print(f"{index:04d}\t{line}")
        print(f"TABLE_COUNT\t{doc.TextTables.Count}")
        for name in doc.TextTables.ElementNames:
            table = doc.TextTables.getByName(name)
            print(f"TABLE\t{name}\t{len(table.Rows)}x{len(table.Columns)}")
    finally:
        doc.close(True)


if __name__ == "__main__":
    main()
