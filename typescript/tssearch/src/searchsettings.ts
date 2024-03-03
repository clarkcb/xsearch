/*
 * searchsettings.ts
 *
 * represents the settings to use when performing the search
 */

"use strict";

import {FileType, FindSettings, SortBy, SortUtil, StringUtil} from 'tsfind';

export class SearchSettings {
    private readonly findSettings: FindSettings;
    colorize = true;
    firstMatch = false;
    inLinesAfterPatterns: RegExp[] = [];
    inLinesBeforePatterns: RegExp[] = [];
    linesAfter = 0;
    linesAfterToPatterns: RegExp[] = [];
    linesAfterUntilPatterns: RegExp[] = [];
    linesBefore = 0;
    maxLineLength = 150;
    multilineSearch = false;
    outLinesAfterPatterns: RegExp[] = [];
    outLinesBeforePatterns: RegExp[] = [];
    printLines = false;
    printResults = false;
    searchArchives = false;
    searchPatterns: RegExp[] = [];
    #textFileEncoding: BufferEncoding = "utf-8";
    uniqueLines = false;

    constructor() {
        this.findSettings = new FindSettings();
    }

    public getFindSettings(): FindSettings {
        return this.findSettings;
    }

    private static addPatterns(patterns: string|string[], arr: RegExp[]): void {
        if (typeof(patterns) === 'string') {
            arr.push(new RegExp(patterns));
        } else if (patterns.constructor === Array) {
            patterns.forEach((p: string) => arr.push(new RegExp(p)));
        }
    }

    public get archivesOnly(): boolean {
        return this.findSettings.archivesOnly;
    }

    public set archivesOnly(value: boolean) {
        this.findSettings.archivesOnly = value;
        //if (value) this.findSettings.includeArchives = value;
        if (value) this.searchArchives = value;
    }

    public get debug(): boolean {
        return this.findSettings.debug;
    }

    public set debug(value: boolean) {
        this.findSettings.debug = value;
        if (value) this.findSettings.verbose = value;
    }

    get includeHidden(): boolean {
        return this.findSettings.includeHidden;
    }

    set includeHidden(value: boolean) {
        this.findSettings.includeHidden = value;
    }

    get inArchiveExtensions(): string[] {
        return this.findSettings.inArchiveExtensions;
    }

    set inArchiveExtensions(value: string[]) {
        this.findSettings.inArchiveExtensions = value;
    }

    public addInArchiveExtensions(ext: string|string[]): void {
        this.findSettings.addInArchiveExtensions(ext);
    }

    get inArchiveFilePatterns(): RegExp[] {
        return this.findSettings.inArchiveFilePatterns;
    }

    set inArchiveFilePatterns(value: RegExp[]) {
        this.findSettings.inArchiveFilePatterns = value;
    }

    public addInArchiveFilePatterns(pattern: string|string[]): void {
        this.findSettings.addInArchiveFilePatterns(pattern);
    }

    get inDirPatterns(): RegExp[] {
        return this.findSettings.inDirPatterns;
    }

    set inDirPatterns(value: RegExp[]) {
        this.findSettings.inDirPatterns = value;
    }

    public addInDirPatterns(pattern: string|string[]): void {
        this.findSettings.addInDirPatterns(pattern);
    }

    get inExtensions(): string[] {
        return this.findSettings.inExtensions;
    }

    set inExtensions(value: string[]) {
        this.findSettings.inExtensions = value;
    }

    public addInExtensions(ext: string|string[]): void {
        this.findSettings.addInExtensions(ext);
    }

    get inFilePatterns(): RegExp[] {
        return this.findSettings.inFilePatterns;
    }

    set inFilePatterns(value: RegExp[]) {
        this.findSettings.inFilePatterns = value;
    }

    public addInFilePatterns(pattern: string|string[]): void {
        this.findSettings.addInFilePatterns(pattern);
    }

    get inFileTypes(): FileType[] {
        return this.findSettings.inFileTypes;
    }

    set inFileTypes(value: FileType[]) {
        this.findSettings.inFileTypes = value;
    }

    public addInFileTypes(fileType: string|string[]): void {
        this.findSettings.addInFileTypes(fileType);
    }

    public addInLinesAfterPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.inLinesAfterPatterns);
    }

    public addInLinesBeforePatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.inLinesBeforePatterns);
    }

    public addLinesAfterToPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.linesAfterToPatterns);
    }

    public addLinesAfterUntilPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.linesAfterUntilPatterns);
    }

    get maxLastMod(): Date | null {
        return this.findSettings.maxLastMod;
    }

    get maxDepth(): number {
        return this.findSettings.maxDepth;
    }

    set maxDepth(value: number) {
        this.findSettings.maxDepth = value;
    }

    set maxLastMod(value: Date | null) {
        this.findSettings.maxLastMod = value;
    }

    maxLastModFromString(s: string) {
        this.findSettings.maxLastMod = StringUtil.getDateForString(s);
    }

    get maxSize(): number {
        return this.findSettings.maxSize;
    }

    set maxSize(value: number) {
        this.findSettings.maxSize = value;
    }

    get minDepth(): number {
        return this.findSettings.minDepth;
    }

    set minDepth(value: number) {
        this.findSettings.minDepth = value;
    }

    get minLastMod(): Date | null {
        return this.findSettings.minLastMod;
    }

    set minLastMod(value: Date | null) {
        this.findSettings.minLastMod = value;
    }

    minLastModFromString(s: string) {
        this.findSettings.minLastMod = StringUtil.getDateForString(s);
    }

    get minSize(): number {
        return this.findSettings.minSize;
    }

    set minSize(value: number) {
        this.findSettings.minSize = value;
    }

    get outArchiveExtensions(): string[] {
        return this.findSettings.outArchiveExtensions;
    }

    set outArchiveExtensions(value: string[]) {
        this.findSettings.outArchiveExtensions = value;
    }

    public addOutArchiveExtensions(ext: string|string[]): void {
        this.findSettings.addOutArchiveExtensions(ext);
    }

    get outArchiveFilePatterns(): RegExp[] {
        return this.findSettings.outArchiveFilePatterns;
    }

    set outArchiveFilePatterns(value: RegExp[]) {
        this.findSettings.outArchiveFilePatterns = value;
    }

    public addOutArchiveFilePatterns(pattern: string|string[]): void {
        this.findSettings.addOutArchiveFilePatterns(pattern);
    }

    get outDirPatterns(): RegExp[] {
        return this.findSettings.outDirPatterns;
    }

    set outDirPatterns(value: RegExp[]) {
        this.findSettings.outDirPatterns = value;
    }

    public addOutDirPatterns(pattern: string|string[]): void {
        this.findSettings.addOutDirPatterns(pattern);
    }

    get outExtensions(): string[] {
        return this.findSettings.outExtensions;
    }

    set outExtensions(value: string[]) {
        this.findSettings.outExtensions = value;
    }

    public addOutExtensions(ext: string|string[]): void {
        this.findSettings.addOutExtensions(ext);
    }

    get outFilePatterns(): RegExp[] {
        return this.findSettings.outFilePatterns;
    }

    set outFilePatterns(value: RegExp[]) {
        this.findSettings.outFilePatterns = value;
    }

    public addOutFilePatterns(pattern: string|string[]): void {
        this.findSettings.addOutFilePatterns(pattern);
    }

    get outFileTypes(): FileType[] {
        return this.findSettings.outFileTypes;
    }

    set outFileTypes(value: FileType[]) {
        this.findSettings.outFileTypes = value;
    }

    public addOutFileTypes(fileType: string|string[]): void {
        this.findSettings.addOutFileTypes(fileType);
    }

    public addOutLinesAfterPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.outLinesAfterPatterns);
    }

    public addOutLinesBeforePatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.outLinesBeforePatterns);
    }

    get paths(): string[] {
        return this.findSettings.paths;
    }

    set paths(value: string[]) {
        this.findSettings.paths = value;
    }

    get printDirs(): boolean {
        return this.findSettings.printDirs;
    }

    set printDirs(value: boolean) {
        this.findSettings.printDirs = value;
    }

    get printFiles(): boolean {
        return this.findSettings.printFiles;
    }

    set printFiles(value: boolean) {
        this.findSettings.printFiles = value;
    }

    get printUsage(): boolean {
        return this.findSettings.printUsage;
    }

    set printUsage(value: boolean) {
        this.findSettings.printUsage = value;
    }

    get printVersion(): boolean {
        return this.findSettings.printVersion;
    }

    set printVersion(value: boolean) {
        this.findSettings.printVersion = value;
    }

    get recursive(): boolean {
        return this.findSettings.recursive;
    }

    set recursive(value: boolean) {
        this.findSettings.recursive = value;
    }

    public addSearchPatterns(pattern: string|string[]): void {
        SearchSettings.addPatterns(pattern, this.searchPatterns);
    }

    get sortBy(): SortBy {
        return this.findSettings.sortBy;
    }

    set sortBy(value: SortBy) {
        this.findSettings.sortBy = value;
    }

    get sortCaseInsensitive(): boolean {
        return this.findSettings.sortCaseInsensitive;
    }

    set sortCaseInsensitive(value: boolean) {
        this.findSettings.sortCaseInsensitive = value;
    }

    get sortDescending(): boolean {
        return this.findSettings.sortDescending;
    }

    set sortDescending(value: boolean) {
        this.findSettings.sortDescending = value;
    }

    get textFileEncoding(): BufferEncoding {
        return this.#textFileEncoding;
    }

    set textFileEncoding(value: string) {
        this.#textFileEncoding = value as BufferEncoding;
    }

    get verbose(): boolean {
        return this.findSettings.verbose;
    }

    set verbose(value: boolean) {
        this.findSettings.verbose = value;
    }

    public toString(): string {
        return 'SearchSettings('
            + 'archivesOnly=' + this.archivesOnly
            + ', colorize=' + this.colorize
            + ', debug=' + this.debug
            + ', firstMatch=' + this.firstMatch
            + ', ' + StringUtil.stringListToString('inArchiveExtensions', this.inArchiveExtensions)
            + ', ' + StringUtil.patternListToString('inArchiveFilePatterns', this.inArchiveFilePatterns)
            + ', includeHidden=' + this.includeHidden
            + ', ' + StringUtil.patternListToString('inDirPatterns', this.inDirPatterns)
            + ', ' + StringUtil.stringListToString('inExtensions', this.inExtensions)
            + ', ' + StringUtil.patternListToString('inFilePatterns', this.inFilePatterns)
            + ', ' + StringUtil.fileTypesToString('inFileTypes', this.inFileTypes)
            + ', ' + StringUtil.patternListToString('inLinesAfterPatterns', this.inLinesAfterPatterns)
            + ', ' + StringUtil.patternListToString('inLinesBeforePatterns', this.inLinesBeforePatterns)
            + ', linesAfter=' + this.linesAfter
            + ', ' + StringUtil.patternListToString('linesAfterToPatterns', this.linesAfterToPatterns)
            + ', ' + StringUtil.patternListToString('linesAfterUntilPatterns', this.linesAfterUntilPatterns)
            + ', linesBefore=' + this.linesBefore
            + ', maxDepth=' + this.maxDepth
            + ', ' + StringUtil.dateToString('maxLastMod', this.maxLastMod)
            + ', maxLineLength=' + this.maxLineLength
            + ', maxSize=' + this.maxSize
            + ', minDepth=' + this.minDepth
            + ', ' + StringUtil.dateToString('minLastMod', this.minLastMod)
            + ', minSize=' + this.minSize
            + ', multilineSearch=' + this.multilineSearch
            + ', ' + StringUtil.stringListToString('outArchiveExtensions', this.outArchiveExtensions)
            + ', ' + StringUtil.patternListToString('outArchiveFilePatterns', this.outArchiveFilePatterns)
            + ', ' + StringUtil.patternListToString('outDirPatterns', this.outDirPatterns)
            + ', ' + StringUtil.stringListToString('outExtensions', this.outExtensions)
            + ', ' + StringUtil.patternListToString('outFilePatterns', this.outFilePatterns)
            + ', ' + StringUtil.fileTypesToString('outFileTypes', this.outFileTypes)
            + ', ' + StringUtil.patternListToString('outLinesAfterPatterns', this.outLinesAfterPatterns)
            + ', ' + StringUtil.patternListToString('outLinesBeforePatterns', this.outLinesBeforePatterns)
            + ', ' + StringUtil.stringListToString('paths', this.paths)
            + ', printDirs=' + this.printDirs
            + ', printFiles=' + this.printFiles
            + ', printLines=' + this.printLines
            + ', printResults=' + this.printResults
            + ', printVersion=' + this.printVersion
            + ', recursive=' + this.recursive
            + ', searchArchives=' + this.searchArchives
            + ', ' + StringUtil.patternListToString('searchPatterns', this.searchPatterns)
            + ', sortBy=' + SortUtil.sortByToName(this.sortBy)
            + ', sortCaseInsensitive=' + this.sortCaseInsensitive
            + ', sortDescending=' + this.sortDescending
            + ', textFileEncoding="' + this.textFileEncoding + '"'
            + ', uniqueLines=' + this.uniqueLines
            + ', verbose=' + this.verbose
            + ')';
    }
}
