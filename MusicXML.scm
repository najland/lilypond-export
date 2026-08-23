;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%                                                                             %
;% This file is part of openLilyLib,                                           %
;%                      ===========                                            %
;% the community library project for GNU LilyPond                              %
;% (https://github.com/openlilylib)                                            %
;%              -----------                                                    %
;%                                                                             %
;% Library: lilypond-export                                                    %
;%          ===============                                                    %
;%                                                                             %
;% export foreign file formats with LilyPond                                   %
;%                                                                             %
;% lilypond-export is free software: you can redistribute it and/or modify     %
;% it under the terms of the GNU General Public License as published by        %
;% the Free Software Foundation, either version 3 of the License, or           %
;% (at your option) any later version.                                         %
;%                                                                             %
;% lilypond-export is distributed in the hope that it will be useful,          %
;% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
;% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
;% GNU General Public License for more details.                                %
;%                                                                             %
;% You should have received a copy of the GNU General Public License           %
;% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
;%                                                                             %
;% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
;% lilypond-export is maintained by Jan-Peter Voigt, jp.voigt@gmx.de           %
;%                                                                             %
;%       Copyright Jan-Peter Voigt, Urs Liska, 2017                            %
;%                                                                             %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;% TODO ties, slurs, grace notes

(define-module (lilypond-export MusicXML))

(use-modules
 (oll-core tree)
 (oll-core internal music-tools)
 (lilypond-export api)
 (lily))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; musicXML export

; number of sharps (positive) or flats (negative) implied by a LilyPond
; keyAlterations alist (list of (degree . alteration), alteration in whole-
; tone units: 1/2 = one sharp/flat). Standard diatonic keys only ever alter
; degrees by the same amount in the same direction, so twice the sum of all
; alterations is exactly the signed sharp/flat count MusicXML expects.
(define (keyalist->fifths alist)
  (inexact->exact (round (* 2 (apply + (map cdr alist))))))


(define (duration-factor dur)
  (*
   (/ 4 (expt 2 (ly:duration-log dur)))
   (duration-dot-factor (ly:duration-dot-count dur))
   (ly:duration-scale dur)
   ))

(define-public (exportMusicXML musicexport filename . options)
  (let ((grid (tree-create 'grid))
        (bar-list (sort (filter integer? (tree-get-keys musicexport '())) (lambda (a b) (< a b))) )
        (finaltime (tree-get musicexport '(finaltime)))
        (division-dur (tree-get musicexport '(division-dur)))
        (divisions 1))
    (define notenames '(C D E F G A B))
    (define types '("breve" "breve" "whole" "half" "quarter" "eighth" "16th" "32nd" "64th" "128th"))
    (define (writeln x . args) (if (> (length args) 0) (apply format #t x args)(display x))(newline))
    (define (writepitch p)
      (if (ly:pitch? p)
          (let ((notename (list-ref notenames (ly:pitch-notename p)))
                (alter (* 2 (ly:pitch-alteration p)))
                (octave (+ 4 (ly:pitch-octave p))))
            (writeln "<pitch>")
            (writeln "<step>~A</step>" notename)
            (if (not (= 0 alter)) (writeln "<alter>~A</alter>" alter))
            (writeln "<octave>~A</octave>" octave)
            (writeln "</pitch>")
            ) (writeln "<rest />")))
    (define (writeduration dur moment)
      (if (ly:duration? dur)
          (let ((divlen (* (duration-factor dur) divisions))
                (divmom (* divisions 4 (ly:moment-main moment)))
                (addskew 0))
            (if (not (integer? divmom))
                (let* ((num (numerator divmom))
                       (den (denominator divmom))
                       (rest (modulo num den))
                       (div (/ (- num rest) den)))
                  ;(ly:message "mom: ~A ~A" (/ div rest) rest)
                  (set! addskew (/ rest den))
                  ))
            ;(ly:message "dur: ~A" (* divlen divisions))
            (if (not (integer? divlen))
                (let* ((len (inexact->exact divlen))
                       (num (numerator len))
                       (den (denominator len))
                       (rest (modulo num den))
                       (dur (/ (- num rest) den))
                       (adddur (+ addskew (/ rest den))))
                  (while (>= adddur 1)
                    (set! dur (1+ dur))
                    (set! adddur (1- adddur)))
                  ;(ly:message "time: ~A:~A ... ~A" num den rest)
                  (set! divlen dur)
                  ))
            (writeln "<duration>~A</duration>" divlen)
            )))
    (define (writetype dur)
      (if (ly:duration? dur)
          (writeln "<type>~A</type>" (list-ref types (+ 2 (ly:duration-log dur))))
          ))
    (define (writedots d) (if (> d 0) (begin (writeln "<dot/>")(writedots (1- d)))))
    (define (writetimemod dur)
      (if (and (ly:duration? dur) (not (integer? (ly:duration-scale dur))))
          (let ((num (numerator (ly:duration-scale dur)))
                (den (denominator (ly:duration-scale dur))))
            (writeln "<time-modification>")
            (writeln "<actual-notes>~A</actual-notes>" den)
            (writeln "<normal-notes>~A</normal-notes>" num)
            (writeln "</time-modification>")
            )))
    (define (writetuplet tuplet)
      (if (pair? tuplet)
          (begin
           (writeln "<notations>")
           (writeln "<tuplet number=\"1\" placement=\"above\" type=\"~A\" />" (car tuplet))
           (writeln "</notations>")
           )))
    (define (writeslurs marks)
      (for-each (lambda (m) (writeln "<notations><slur type=\"~A\" number=\"1\"/></notations>" (if (eq? m 'start) "start" "stop")))
        (if (list? marks) marks '())))
    (define (writefermata has-fermata)
      (if has-fermata (writeln "<notations><fermata/></notations>")))
    ; maps LilyPond articulation-type symbols to (xml-group . xml-tag);
    ; anything not in this table is silently skipped
    (define articulation-xml-map
      '((accent . ("articulations" . "accent"))
        (marcato . ("articulations" . "strong-accent"))
        (staccato . ("articulations" . "staccato"))
        (staccatissimo . ("articulations" . "staccatissimo"))
        (tenuto . ("articulations" . "tenuto"))
        (portato . ("articulations" . "detached-legato"))
        (stress . ("articulations" . "stress"))
        (unstress . ("articulations" . "unstress"))
        (upbow . ("technical" . "up-bow"))
        (downbow . ("technical" . "down-bow"))
        (open . ("technical" . "open-string"))
        (stopped . ("technical" . "stopped"))
        (flageolet . ("technical" . "harmonic"))
        (snappizzicato . ("technical" . "snap-pizzicato"))
        (trill . ("ornaments" . "trill-mark"))
        (turn . ("ornaments" . "turn"))
        (mordent . ("ornaments" . "mordent"))
        (prallmordent . ("ornaments" . "inverted-mordent"))))
    (define (writearticulations marks)
      (if (and (list? marks) (pair? marks))
          (let ((groups (list (cons "articulations" '()) (cons "technical" '()) (cons "ornaments" '()))))
            (for-each
             (lambda (m)
               (let ((entry (assq m articulation-xml-map)))
                 (if entry
                     (let* ((group (cadr entry)) (tag (cddr entry))
                            (cell (assoc group groups)))
                       (set-cdr! cell (cons tag (cdr cell)))))))
             marks)
            (if (or (pair? (cdr (assoc "articulations" groups)))
                    (pair? (cdr (assoc "technical" groups)))
                    (pair? (cdr (assoc "ornaments" groups))))
                (begin
                 (writeln "<notations>")
                 (for-each
                  (lambda (g)
                    (if (pair? (cdr g))
                        (begin
                         (writeln "<~A>" (car g))
                         (for-each (lambda (tag) (writeln "<~A/>" tag)) (cdr g))
                         (writeln "</~A>" (car g)))))
                  groups)
                 (writeln "</notations>"))))))
    (define (writetie marks)
      ; <tie> (duration/playback) goes right after <duration>; <tied>
      ; (the visual notation) goes in <notations>, written by writemusic
      (for-each (lambda (m) (writeln "<tie type=\"~A\"/>" (if (eq? m 'start) "start" "stop")))
        (if (list? marks) marks '())))
    (define (writetied marks)
      (for-each (lambda (m) (writeln "<notations><tied type=\"~A\"/></notations>" (if (eq? m 'start) "start" "stop")))
        (if (list? marks) marks '())))
    (define (acctext accidental)
      (case accidental
        ((0) "natural")
        ((-1/2) "flat")
        ((1/2) "sharp")
        ((-1) "flat-flat")
        ((1) "double-sharp")
        (else "")))
    (define (writemusic m staff voice . opts)
      (let ((dur (ly:music-property m 'duration))
            (chord (ly:assoc-get 'chord opts #f #f))
            (accidental (ly:assoc-get 'accidental opts #f #f))
            (beam (ly:assoc-get 'beam opts))
            (tuplet (ly:assoc-get 'tuplet opts))
            (lyrics (ly:assoc-get 'lyrics opts))
            (slur (ly:assoc-get 'slur opts))
            (fermata (ly:assoc-get 'fermata opts))
            (articulations (ly:assoc-get 'articulations opts))
            (tie (ly:assoc-get 'tie opts))
            (moment (ly:assoc-get 'moment opts)))
;(ly:message "-----> lyrics ~A" lyrics)
        (case (ly:music-property m 'name)

          ((NoteEvent)
           (writeln "<note>")
           (if chord (writeln "<chord />"))
           (writepitch (ly:music-property m 'pitch))
           (writeduration dur moment)
           (writetie (if chord '() tie))

           (writeln "<voice>~A</voice>" voice)
           (writetype dur)
           (if accidental
               (writeln "<accidental>~A</accidental>" (acctext accidental)))
           (writedots (if (ly:duration? dur) (ly:duration-dot-count dur) 0))

           (if (symbol? beam) (writeln "<beam number=\"1\">~A</beam>" beam))
           (writetimemod dur)
           (writetuplet tuplet)
           (writeslurs (if chord '() slur))
           (writefermata (and (not chord) fermata))
           (writearticulations (if chord '() articulations))
           (writetied (if chord '() tie))
           (if (and (not chord) (list? lyrics))
               (for-each
                (lambda (indexed-lyric)
                  ;(ly:message "~A" indexed-lyric)
                  (let* ((entry (list-ref indexed-lyric 0))
                         (num (list-ref indexed-lyric 1))
                         ; entry is (text preceded-by-hyphen? box), where
                         ; (car box) tells whether this syllable is followed
                         ; by a hyphen. Older/foreign entries may just be a
                         ; plain string; fall back to "single" for those.
                         (text (if (pair? entry) (car entry) entry))
                         (preceded (and (pair? entry) (list-ref entry 1)))
                         (box (and (pair? entry) (list-ref entry 2)))
                         (followed (and (pair? box) (car box)))
                         (syllabic (cond
                                    ((and preceded followed) "middle")
                                    (preceded "end")
                                    (followed "begin")
                                    (else "single"))))
                    (writeln "<lyric number=\"~A\"><syllabic>~A</syllabic><text>~A</text></lyric>"
                      (+ 1 num) syllabic text))
                  ) (map list lyrics (iota (length lyrics)))))

           (writeln "</note>"))

          ((RestEvent)
           (let ((wholemeasure (ly:music-property m 'measure-rest #f)))
             (writeln "<note>")
             (writeln (if wholemeasure "<rest measure=\"yes\" />" "<rest />"))
             (writeduration dur moment)

             (writeln "<voice>~A</voice>" voice)
             (if (not wholemeasure)
                 (begin
                  (writetype dur)
                  (writedots (if (ly:duration? dur) (ly:duration-dot-count dur) 0))))
             (writetimemod dur)
             (writetuplet tuplet)
             (writeln "</note>")))

          ((EventChord)
           (let* ((elements (ly:music-property m 'elements))
                  (notes (filter (lambda (m) (music-is? m 'NoteEvent)) elements))
                  (note-count (length notes))
                  (artics (filter (lambda (m) (not (music-is? m 'NoteEvent))) elements)))
             (if (> note-count 0) (apply writemusic (car notes) staff voice opts))
             ;(set! opts (assoc-remove! opts 'beam))
             (for-each
              (lambda (n)
                (apply writemusic n staff voice (cons '(chord . #t) opts))
                ) (cdr notes))
             ))

          )))

    (if (ly:duration? division-dur) (set! divisions (/ 64 (duration-factor division-dur))))
    (ly:message "divisions: ~A" divisions)

    (tree-walk musicexport '()
      (lambda (path key value)
        (if (= 4 (length path))
            (let ((staff (caddr path))
                  (voice (cadddr path)))
              (if (and (integer? staff)(integer? voice))
                  (tree-set! grid (list staff voice) #t))
              )
            )))

    (let ((staff-list (sort (tree-get-keys grid '()) (lambda (a b) (< a b)))))
      (with-output-to-file filename
        (lambda ()
          (writeln "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>")
          (writeln "<!DOCTYPE score-partwise PUBLIC \"-//Recordare//DTD MusicXML 3.0 Partwise//EN\" \"http://www.musicxml.org/dtds/partwise.dtd\">")
          (writeln "<score-partwise version=\"3.0\">")
          (writeln "<part-list>")
          (for-each
           (lambda (staff)
             (let* ((pname (tree-get musicexport (list 'partname staff)))
                    (name (if (and (pair? pname) (string? (car pname))) (car pname) (format #f "Part ~A" staff)))
                    (abbr (and (pair? pname) (string? (cdr pname)) (cdr pname))))
               (writeln "<score-part id=\"P~A\">" staff)
               (writeln "<part-name>~A</part-name>" name)
               (if abbr (writeln "<part-abbreviation>~A</part-abbreviation>" abbr))
               (writeln "</score-part>"))
             ) staff-list)
          (writeln "</part-list>")

          (for-each
           (lambda (staff)
             (define (writeclef measure moment doattr)
               (let ((clefGlyph (tree-get musicexport (list measure moment staff 'clefGlyph)))
                     (clefPosition (tree-get musicexport (list measure moment staff 'clefPosition)))
                     (clefTransposition (tree-get musicexport (list measure moment staff 'clefTransposition))))
                 (if (and (string? clefGlyph)(integer? clefPosition))
                     (begin
                      (if doattr (writeln "<attributes>"))
                      (writeln "<clef><sign>~A</sign><line>~A</line>~A</clef>"
                        (list-ref (string-split clefGlyph #\.) 1)
                        (+ 3 (/ clefPosition 2))
                        (if (and (not (= 0 clefTransposition))(= 0 (modulo clefTransposition 7)))
                            (format #f "<clef-octave-change>~A</clef-octave-change>" (/ clefTransposition 7))
                            ""))
                      (if doattr (writeln "</attributes>"))
                      ))))

             (define (writekey measure moment doattr)
               (let ((keydata (tree-get musicexport (list measure moment staff 'keysig))))
                 (if (list? keydata)
                     (let ((fifths (keyalist->fifths keydata)))
                       (if doattr (writeln "<attributes>"))
                       (writeln "<key><fifths>~A</fifths></key>" fifths)
                       (if doattr (writeln "</attributes>"))
                       ))))

             ; \break/\pageBreak are global (not per-staff), so only the
             ; first part carries the <print> tag -- that's the convention
             ; most software follows and is enough for correct display
             (define (writebreak measure)
               (if (= staff (car staff-list))
                   (cond
                    ((tree-get musicexport (list measure (ly:make-moment 0) 'pagebreak))
                     (writeln "<print new-page=\"yes\"/>"))
                    ((tree-get musicexport (list measure (ly:make-moment 0) 'linebreak))
                     (writeln "<print new-system=\"yes\"/>")))))

             ; \tempo is global (not per staff), so only the first part
             ; carries it, same convention as writebreak above
             (define (writetempo measure moment)
               (if (= staff (car staff-list))
                   (let ((tempodata (tree-get musicexport (list measure moment 'tempo))))
                     (if (list? tempodata)
                         (let ((text (list-ref tempodata 0))
                               (count (list-ref tempodata 1))
                               (unit (list-ref tempodata 2)))
                           (writeln "<direction placement=\"above\">")
                           (if (and (ly:duration? unit) count)
                               (writeln "<direction-type><metronome><beat-unit>~A</beat-unit><per-minute>~A</per-minute></metronome></direction-type>"
                                 (list-ref types (+ 2 (ly:duration-log unit))) count))
                           (if (string? text)
                               (writeln "<direction-type><words>~A</words></direction-type>" text))
                           (if (and (ly:duration? unit) count)
                               (writeln "<sound tempo=\"~A\"/>" (inexact->exact (round (* count (duration-factor unit))))))
                           (writeln "</direction>"))))))

             (writeln "<part id=\"P~A\">" staff)

             (for-each
              (lambda (measure)
                (let ((backup 0)
                      (beamcont #f)
                      (moment-list (sort (filter ly:moment? (tree-get-keys musicexport (list measure))) ly:moment<?))
                      (first-moment (ly:make-moment 0)))

                  (if (> (length moment-list) 0) (set! first-moment (car moment-list)))

                  (writeln "<measure number=\"~A\">" measure)
                  (writebreak measure)

                  (writeln "<attributes>")
                  (writeln "<divisions>~A</divisions>" divisions) ; divisions by measure?
                  (writekey measure first-moment #f)
                  (let ((meter (tree-get musicexport (list measure first-moment staff 'timesig))))
                    (if (number-pair? meter)
                        (writeln "<time><beats>~A</beats><beat-type>~A</beat-type></time>" (car meter)(cdr meter))))
                  (writeclef measure first-moment #f)
                  (writeln "</attributes>")
                  (writetempo measure first-moment)

                  (for-each
                   (lambda (voice)
                     (if (> backup 0) (writeln "<backup><duration>~A</duration></backup>" backup))
                     (set! backup 0)
                     (for-each
                      (lambda (moment)
                        (let ((music (tree-get musicexport (list measure moment staff voice))))
                          (if (not (equal? moment (ly:make-moment 0)))
                              (begin
                               (writekey measure moment #t)
                               (writeclef measure moment #t)
                               (writetempo measure moment)))
                          (if (ly:music? music)
                              (let ((dur (ly:music-property music 'duration))
                                    (beam (tree-get musicexport (list measure moment staff voice 'beam)))
                                    (accidental (tree-get musicexport (list measure moment staff voice 'accidental)))
                                    (tuplet (tree-get musicexport (list measure moment staff voice 'tuplet)))
                                    (lyrics (tree-get musicexport (list measure moment staff voice 'lyrics)))
                                    (slur (tree-get musicexport (list measure moment staff voice 'slur)))
                                    (fermata (tree-get musicexport (list measure moment staff voice 'fermata)))
                                    (articulations (tree-get musicexport (list measure moment staff voice 'articulations)))
                                    (tie (tree-get musicexport (list measure moment staff voice 'tie)))
                                    )
                                (case beam
                                  ((start) (set! beamcont 'continue))
                                  ((end) (set! beamcont #f))
                                  )

                                ; TODO staff grouping!
                                (writemusic music 1 voice
                                  `(beam . ,(cond
                                             ((eq? 'start beam) 'begin)
                                             ((symbol? beam) beam)
                                             ((symbol? beamcont) beamcont)))
                                  `(accidental . ,accidental)
                                  `(moment . ,moment)
                                  `(tuplet . ,tuplet)
                                  `(lyrics . ,lyrics)
                                  `(slur . ,slur)
                                  `(fermata . ,fermata)
                                  `(articulations . ,articulations)
                                  `(tie . ,tie))
                                (if (ly:duration? dur)
                                    (set! backup (+ backup (* (duration-factor dur) divisions))))
                                ))
                          )) moment-list)
                     ) (sort (tree-get-keys grid (list staff)) (lambda (a b) (< a b))))

                  (writeln "</measure>")
                  )) (sort (filter integer? (tree-get-keys musicexport '())) (lambda (a b) (< a b))))

             (writeln "</part>")
             ) staff-list)

          (writeln "</score-partwise>")
          )))
    ))

(set-object-property! exportMusicXML 'file-suffix "xml")
