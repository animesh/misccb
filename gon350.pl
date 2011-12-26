/* $Id: gon350.pl,v 0.1alpha 2007/04/16 15:36:44 Zzz Exp $

    Part of BioProlog Logic Programming Resources for Bioinformatics

    Author:	Mauro Di Nuzzo
    E-Mail:	info@prologonlinereference.org
    WWW:	http://www.prologonlinereference.org/bioprolog.psp
    Copyright:	�2005-2007 Prolog Online Reference

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/
substitution_matrix(gon350, [
	('A', 'A', 1.0),
	('A', 'R', -0.2),
	('A', 'N', 0.0),
	('A', 'D', 0.0),
	('A', 'C', 0.4),
	('A', 'Q', 0.0),
	('A', 'E', 0.1),
	('A', 'G', 0.5),
	('A', 'H', -0.4),
	('A', 'I', -0.4),
	('A', 'L', -0.7),
	('A', 'K', -0.1),
	('A', 'M', -0.4),
	('A', 'F', -1.4),
	('A', 'P', 0.3),
	('A', 'S', 0.6),
	('A', 'T', 0.4),
	('A', 'W', -2.4),
	('A', 'Y', -1.4),
	('A', 'V', 0.0),
	('A', '-', -3.5),
	('R', 'A', -0.2),
	('R', 'R', 2.8),
	('R', 'N', 0.4),
	('R', 'D', 0.2),
	('R', 'C', -1.3),
	('R', 'Q', 1.0),
	('R', 'E', 0.6),
	('R', 'G', -0.4),
	('R', 'H', 0.6),
	('R', 'I', -1.5),
	('R', 'L', -1.4),
	('R', 'K', 1.8),
	('R', 'M', -1.1),
	('R', 'F', -2.0),
	('R', 'P', -0.4),
	('R', 'S', 0.1),
	('R', 'T', 0.0),
	('R', 'W', -1.2),
	('R', 'Y', -1.2),
	('R', 'V', -1.2),
	('R', '-', -3.5),
	('N', 'A', 0.0),
	('N', 'R', 0.4),
	('N', 'N', 2.0),
	('N', 'D', 1.5),
	('N', 'C', -1.1),
	('N', 'Q', 0.6),
	('N', 'E', 0.9),
	('N', 'G', 0.5),
	('N', 'H', 0.8),
	('N', 'I', -1.8),
	('N', 'L', -1.9),
	('N', 'K', 0.7),
	('N', 'M', -1.4),
	('N', 'F', -2.0),
	('N', 'P', -0.3),
	('N', 'S', 0.6),
	('N', 'T', 0.4),
	('N', 'W', -2.4),
	('N', 'Y', -1.0),
	('N', 'V', -1.3),
	('N', '-', -3.5),
	('D', 'A', 0.0),
	('D', 'R', 0.2),
	('D', 'N', 1.5),
	('D', 'D', 2.9),
	('D', 'C', -1.9),
	('D', 'Q', 0.8),
	('D', 'E', 1.9),
	('D', 'G', 0.4),
	('D', 'H', 0.5),
	('D', 'I', -2.4),
	('D', 'L', -2.5),
	('D', 'K', 0.6),
	('D', 'M', -1.9),
	('D', 'F', -3.0),
	('D', 'P', -0.2),
	('D', 'S', 0.5),
	('D', 'T', 0.2),
	('D', 'W', -3.5),
	('D', 'Y', -1.8),
	('D', 'V', -1.8),
	('D', '-', -3.5),
	('C', 'A', 0.4),
	('C', 'R', -1.3),
	('C', 'N', -1.1),
	('C', 'D', -1.9),
	('C', 'C', 9.3),
	('C', 'Q', -1.4),
	('C', 'E', -1.7),
	('C', 'G', -1.2),
	('C', 'H', -0.8),
	('C', 'I', -0.5),
	('C', 'L', -0.7),
	('C', 'K', -1.6),
	('C', 'M', -0.4),
	('C', 'F', -0.3),
	('C', 'P', -1.8),
	('C', 'S', 0.1),
	('C', 'T', -0.2),
	('C', 'W', -0.4),
	('C', 'Y', -0.1),
	('C', 'V', 0.2),
	('C', '-', -3.5),
	('Q', 'A', 0.0),
	('Q', 'R', 1.0),
	('Q', 'N', 0.6),
	('Q', 'D', 0.8),
	('Q', 'C', -1.4),
	('Q', 'Q', 1.2),
	('Q', 'E', 1.1),
	('Q', 'G', -0.4),
	('Q', 'H', 0.7),
	('Q', 'I', -1.1),
	('Q', 'L', -1.1),
	('Q', 'K', 1.0),
	('Q', 'M', -0.7),
	('Q', 'F', -1.6),
	('Q', 'P', 0.0),
	('Q', 'S', 0.2),
	('Q', 'T', 0.1),
	('Q', 'W', -1.9),
	('Q', 'Y', -1.1),
	('Q', 'V', -0.9),
	('Q', '-', -3.5),
	('E', 'A', 0.1),
	('E', 'R', 0.6),
	('E', 'N', 0.9),
	('E', 'D', 1.9),
	('E', 'C', -1.7),
	('E', 'Q', 1.1),
	('E', 'E', 2.0),
	('E', 'G', -0.2),
	('E', 'H', 0.4),
	('E', 'I', -1.7),
	('E', 'L', -1.8),
	('E', 'K', 0.9),
	('E', 'M', -1.3),
	('E', 'F', -2.5),
	('E', 'P', 0.0),
	('E', 'S', 0.3),
	('E', 'T', 0.1),
	('E', 'W', -2.9),
	('E', 'Y', -1.7),
	('E', 'V', -1.2),
	('E', '-', -3.5),
	('G', 'A', 0.5),
	('G', 'R', -0.4),
	('G', 'N', 0.5),
	('G', 'D', 0.4),
	('G', 'C', -1.2),
	('G', 'Q', -0.4),
	('G', 'E', -0.2),
	('G', 'G', 5.1),
	('G', 'H', -0.7),
	('G', 'I', -2.9),
	('G', 'L', -3.0),
	('G', 'K', -0.4),
	('G', 'M', -2.3),
	('G', 'F', -3.5),
	('G', 'P', -0.7),
	('G', 'S', 0.5),
	('G', 'T', -0.5),
	('G', 'W', -3.0),
	('G', 'Y', -2.7),
	('G', 'V', -2.1),
	('G', '-', -3.5),
	('H', 'A', -0.4),
	('H', 'R', 0.6),
	('H', 'N', 0.8),
	('H', 'D', 0.5),
	('H', 'C', -0.8),
	('H', 'Q', 0.7),
	('H', 'E', 0.4),
	('H', 'G', -0.7),
	('H', 'H', 3.3),
	('H', 'I', -1.3),
	('H', 'L', -1.1),
	('H', 'K', 0.5),
	('H', 'M', -0.8),
	('H', 'F', 0.1),
	('H', 'P', -0.5),
	('H', 'S', 0.0),
	('H', 'T', -0.1),
	('H', 'W', -0.3),
	('H', 'Y', 1.5),
	('H', 'V', -1.1),
	('H', '-', -3.5),
	('I', 'A', -0.4),
	('I', 'R', -1.5),
	('I', 'N', -1.8),
	('I', 'D', -2.4),
	('I', 'C', -0.5),
	('I', 'Q', -1.1),
	('I', 'E', -1.7),
	('I', 'G', -2.9),
	('I', 'H', -1.3),
	('I', 'I', 2.7),
	('I', 'L', 2.2),
	('I', 'K', -1.3),
	('I', 'M', 1.9),
	('I', 'F', 1.1),
	('I', 'P', -1.6),
	('I', 'S', -1.1),
	('I', 'T', -0.3),
	('I', 'W', -0.9),
	('I', 'Y', -0.1),
	('I', 'V', 2.2),
	('I', '-', -3.5),
	('L', 'A', -0.7),
	('L', 'R', -1.4),
	('L', 'N', -1.9),
	('L', 'D', -2.5),
	('L', 'C', -0.7),
	('L', 'Q', -1.1),
	('L', 'E', -1.8),
	('L', 'G', -3.0),
	('L', 'H', -1.1),
	('L', 'I', 2.2),
	('L', 'L', 2.8),
	('L', 'K', -1.4),
	('L', 'M', 2.1),
	('L', 'F', 1.8),
	('L', 'P', -1.5),
	('L', 'S', -1.3),
	('L', 'T', -0.7),
	('L', 'W', -0.1),
	('L', 'Y', 0.5),
	('L', 'V', 1.6),
	('L', '-', -3.5),
	('K', 'A', -0.1),
	('K', 'R', 1.8),
	('K', 'N', 0.7),
	('K', 'D', 0.6),
	('K', 'C', -1.6),
	('K', 'Q', 1.0),
	('K', 'E', 0.9),
	('K', 'G', -0.4),
	('K', 'H', 0.5),
	('K', 'I', -1.3),
	('K', 'L', -1.4),
	('K', 'K', 1.8),
	('K', 'M', -0.9),
	('K', 'F', -2.1),
	('K', 'P', -0.2),
	('K', 'S', 0.2),
	('K', 'T', 0.2),
	('K', 'W', -2.3),
	('K', 'Y', -1.4),
	('K', 'V', -1.0),
	('K', '-', -3.5),
	('M', 'A', -0.4),
	('M', 'R', -1.1),
	('M', 'N', -1.4),
	('M', 'D', -1.9),
	('M', 'C', -0.4),
	('M', 'Q', -0.7),
	('M', 'E', -1.3),
	('M', 'G', -2.3),
	('M', 'H', -0.8),
	('M', 'I', 1.9),
	('M', 'L', 2.1),
	('M', 'K', -0.9),
	('M', 'M', 2.3),
	('M', 'F', 1.4),
	('M', 'P', -1.4),
	('M', 'S', -0.9),
	('M', 'T', -0.4),
	('M', 'W', -0.3),
	('M', 'Y', 0.2),
	('M', 'V', 1.4),
	('M', '-', -3.5),
	('F', 'A', -1.4),
	('F', 'R', -2.0),
	('F', 'N', -2.0),
	('F', 'D', -3.0),
	('F', 'C', -0.3),
	('F', 'Q', -1.6),
	('F', 'E', -2.5),
	('F', 'G', -3.5),
	('F', 'H', 0.1),
	('F', 'I', 1.1),
	('F', 'L', 1.8),
	('F', 'K', -2.1),
	('F', 'M', 1.4),
	('F', 'F', 5.1),
	('F', 'P', -2.5),
	('F', 'S', -1.8),
	('F', 'T', -1.3),
	('F', 'W', 3.5),
	('F', 'Y', 4.2),
	('F', 'V', 0.5),
	('F', '-', -3.5),
	('P', 'A', 0.3),
	('P', 'R', -0.4),
	('P', 'N', -0.3),
	('P', 'D', -0.2),
	('P', 'C', -1.8),
	('P', 'Q', 0.0),
	('P', 'E', 0.0),
	('P', 'G', -0.7),
	('P', 'H', -0.5),
	('P', 'I', -1.6),
	('P', 'L', -1.5),
	('P', 'K', -0.2),
	('P', 'M', -1.4),
	('P', 'F', -2.5),
	('P', 'P', 5.6),
	('P', 'S', 0.4),
	('P', 'T', 0.2),
	('P', 'W', -3.4),
	('P', 'Y', -2.0),
	('P', 'V', -1.1),
	('P', '-', -3.5),
	('S', 'A', 0.6),
	('S', 'R', 0.1),
	('S', 'N', 0.6),
	('S', 'D', 0.5),
	('S', 'C', 0.1),
	('S', 'Q', 0.2),
	('S', 'E', 0.3),
	('S', 'G', 0.5),
	('S', 'H', 0.0),
	('S', 'I', -1.1),
	('S', 'L', -1.3),
	('S', 'K', 0.2),
	('S', 'M', -0.9),
	('S', 'F', -1.8),
	('S', 'P', 0.4),
	('S', 'S', 1.0),
	('S', 'T', 0.8),
	('S', 'W', -2.2),
	('S', 'Y', -1.2),
	('S', 'V', -0.6),
	('S', '-', -3.5),
	('T', 'A', 0.4),
	('T', 'R', 0.0),
	('T', 'N', 0.4),
	('T', 'D', 0.2),
	('T', 'C', -0.2),
	('T', 'Q', 0.1),
	('T', 'E', 0.1),
	('T', 'G', -0.5),
	('T', 'H', -0.1),
	('T', 'I', -0.3),
	('T', 'L', -0.7),
	('T', 'K', 0.2),
	('T', 'M', -0.4),
	('T', 'F', -1.3),
	('T', 'P', 0.2),
	('T', 'S', 0.8),
	('T', 'T', 1.1),
	('T', 'W', -2.3),
	('T', 'Y', -1.2),
	('T', 'V', 0.0),
	('T', '-', -3.5),
	('W', 'A', -2.4),
	('W', 'R', -1.2),
	('W', 'N', -2.4),
	('W', 'D', -3.5),
	('W', 'C', -0.4),
	('W', 'Q', -1.9),
	('W', 'E', -2.9),
	('W', 'G', -3.0),
	('W', 'H', -0.3),
	('W', 'I', -0.9),
	('W', 'L', -0.1),
	('W', 'K', -2.3),
	('W', 'M', -0.3),
	('W', 'F', 3.5),
	('W', 'P', -3.4),
	('W', 'S', -2.2),
	('W', 'T', -2.3),
	('W', 'W', 12.4),
	('W', 'Y', 3.9),
	('W', 'V', -1.4),
	('W', '-', -3.5),
	('Y', 'A', -1.4),
	('Y', 'R', -1.2),
	('Y', 'N', -1.0),
	('Y', 'D', -1.8),
	('Y', 'C', -0.1),
	('Y', 'Q', -1.1),
	('Y', 'E', -1.7),
	('Y', 'G', -2.7),
	('Y', 'H', 1.5),
	('Y', 'I', -0.1),
	('Y', 'L', 0.5),
	('Y', 'K', -1.4),
	('Y', 'M', 0.2),
	('Y', 'F', 4.2),
	('Y', 'P', -2.0),
	('Y', 'S', -1.2),
	('Y', 'T', -1.2),
	('Y', 'W', 3.9),
	('Y', 'Y', 5.7),
	('Y', 'V', -0.4),
	('Y', '-', -3.5),
	('V', 'A', 0.0),
	('V', 'R', -1.2),
	('V', 'N', -1.3),
	('V', 'D', -1.8),
	('V', 'C', 0.2),
	('V', 'Q', -0.9),
	('V', 'E', -1.2),
	('V', 'G', -2.1),
	('V', 'H', -1.1),
	('V', 'I', 2.2),
	('V', 'L', 1.6),
	('V', 'K', -1.0),
	('V', 'M', 1.4),
	('V', 'F', 0.5),
	('V', 'P', -1.1),
	('V', 'S', -0.6),
	('V', 'T', 0.0),
	('V', 'W', -1.4),
	('V', 'Y', -0.4),
	('V', 'V', 2.1),
	('V', '-', -3.5),
	('-', 'A', -3.5),
	('-', 'R', -3.5),
	('-', 'N', -3.5),
	('-', 'D', -3.5),
	('-', 'C', -3.5),
	('-', 'Q', -3.5),
	('-', 'E', -3.5),
	('-', 'G', -3.5),
	('-', 'H', -3.5),
	('-', 'I', -3.5),
	('-', 'L', -3.5),
	('-', 'K', -3.5),
	('-', 'M', -3.5),
	('-', 'F', -3.5),
	('-', 'P', -3.5),
	('-', 'S', -3.5),
	('-', 'T', -3.5),
	('-', 'W', -3.5),
	('-', 'Y', -3.5),
	('-', 'V', -3.5),
	('-', '-', 1)
]).