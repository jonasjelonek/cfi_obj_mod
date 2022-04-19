Die beiliegende 'config.json' Datei ist nur als Orientierung zu verwenden. Für ein anderes Projekt müssen darin Pfade und eventuell
andere Einstellungen angepasst werden.
In der Konfigurationsdatei wird festgelegt, welche Objektdateien bearbeitet werden sollen und wie und wohin der Checkcode assembliert wird.
Standardmäßig überschreibt cfi_obj_mod die Objektdatei mit der modifizierten Variante.

Das Programm funktioniert grundsätzlich so:
    (1)     Parsen der Konfigurationsdatei um Ausführungsparameter festzulegen
    (2)     Falls spezifiziert, wird der Checkcode assembliert
    <folgender Ablauf wird für jede angegebene Objektdatei durchgeführt>
    (3)     Öffnen und Parsen einer ELF-Objektdatei (andere Formate werden nicht unterstützt)
    (4)     Bearbeiten aller Sections, die mit '.text' im Namen beginnen (also alle Codesections)
        (4.1)   Falls in der Symboltabelle das Symbol für die Checkroutine noch nicht verhanden ist, wird es erstellt und hinzugefügt.
        (4.2)   Suchen aller Rücksprünge in der Section/Funktion. Bei den verwendeten Beispielen war bei der Kompilierung im GCC
                die Option -ffunction-sections gewählt, sodass für jede Funktion eine separate Sektion generiert wird. Somit wird immer
                nur in einer Funktion gesucht. Hat eine Section mehrere Funktionen, wird das auch funktionsweise abgearbeitet.
                Eine Funktion pro Section ist grundsätzlich empfehlenswert und erleichtert die Arbeit des cfi_obj_mod-Tools. Zwar sind auch größere
                Sections mit mehreren Funktionen möglich, dort kann es in einigen Fällen aber zu Problemen kommen.
                Pro Sektion hat man hier also je nach Funktion 1-2 Rücksprünge, die gefunden werden.
        (4.3)   Die Rücksprung wird, je nach Typ, entsprechend modifiziert. Bei den meisten Rücksprüngen führt das dazu, dass zusätzliche Instruktionen
                bzw. Bytes hinzukommen. Bspw. wird bei POP T1 der Rücksprung von 2 auf 4 Byte erweitert und ein Sprung zur Checkroutine eingefügt
        (4.4)   Über die Anpassungen in einer Section hinweg werden die Verschiebungen in einer Shift-Map festgehalten. Da sich durch diese Anpassungen
                Codeadressen verschieben und somit einige Offsets in anderen Instruktionen nicht mehr stimmen, müssen sie angepasst werden.
        (4.5)   Für die gerade bearbeitete Section werden nun für alle Aufrufe, die zur Checkroutine führen, Relokationseinträge erstellt und 
                hinzugefügt. Für den Fall, dass eine Section noch keine Relokationstabelle hat, wird die auch erstellt.
        (4.6)   Sind alle Rücksprünge modifiziert wurden, werden die in einer Funktion enthaltenen Instruktionen sukzessive untersucht. 
                Es müssen folgende Instruktionstypen angepasst werden: LDR (mit PC), B, BL, CBZ/CBNZ. Diese Instruktionen enthalten
                immer einen PC-relative Offset und referenzieren Stellen im Code, die sich durch die Anpassungen verschoben haben.
                Anhand der vorher erstellten Shift-Map werden die Instruktionen entsprechend angepasst, sodass der Code später auch weiterhin 
                problemlos läuft. 
        (4.7)   Durch die Anpassungen verschieben sich auch möglicherweise nachfolgende Symbole, insbesondere wenn eine Sektion nicht nur
                eine Funktion enthält, sondern mehrere oder noch Daten. In diesem Schritt wird der Offset der entsprechenden Symbolen angepasst 
                damit sie auch weiterhin auf die richtigen Stellen zeigen.
        (4.8)   Durch Verschiebungen innerhalb der Funktion kommt es auch dazu, dass bestehende Relokationseinträge nicht mehr stimmen.
                Sie müssen hier ebenfalls angepasst werden um vom Linker richtig aufgelöst werden zu können.
    (5)     Wurde der Prozess für alle relevanten Sections durchgeführt, wird das interne ELF-Objekt wieder in den passenden Bytestrom 
            umgewandelt und dann in die Datei zurückgeschrieben