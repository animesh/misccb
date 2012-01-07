/* $Id: bin45.pl,v 0.1alpha 2007/04/16 15:36:42 Zzz Exp $

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
substitution_matrix(bin45, [
	('C', 'C', 3.29),
	('C', 'S', -0.24),
	('C', 'T', -0.57),
	('C', 'P', -1.14),
	('C', 'A', -0.03),
	('C', 'G', -1.87),
	('C', 'N', -1.1),
	('C', 'D', -1.62),
	('C', 'E', -1.51),
	('C', 'Q', -1.28),
	('C', 'H', -0.58),
	('C', 'R', -0.96),
	('C', 'K', -0.98),
	('C', 'M', -0.21),
	('C', 'I', -0.63),
	('C', 'L', -0.83),
	('C', 'V', -0.22),
	('C', 'F', -0.39),
	('C', 'Y', -0.48),
	('C', 'W', -2.06),
	('C', '-', -0.38),
	('S', 'C', -0.24),
	('S', 'S', 1.63),
	('S', 'T', 0.52),
	('S', 'P', -0.52),
	('S', 'A', 0.3),
	('S', 'G', -0.34),
	('S', 'N', 0.08),
	('S', 'D', -0.1),
	('S', 'E', -0.22),
	('S', 'Q', -0.05),
	('S', 'H', -0.43),
	('S', 'R', -0.34),
	('S', 'K', -0.21),
	('S', 'M', -0.82),
	('S', 'I', -1.41),
	('S', 'L', -1.28),
	('S', 'V', -1.03),
	('S', 'F', -1.32),
	('S', 'Y', -0.95),
	('S', 'W', -2.03),
	('S', '-', 0.29),
	('T', 'C', -0.57),
	('T', 'S', 0.52),
	('T', 'T', 1.72),
	('T', 'P', -0.7),
	('T', 'A', -0.05),
	('T', 'G', -0.96),
	('T', 'N', -0.09),
	('T', 'D', -0.3),
	('T', 'E', -0.29),
	('T', 'Q', -0.05),
	('T', 'H', -0.57),
	('T', 'R', -0.3),
	('T', 'K', -0.19),
	('T', 'M', -0.18),
	('T', 'I', -0.41),
	('T', 'L', -0.77),
	('T', 'V', -0.19),
	('T', 'F', -1.06),
	('T', 'Y', -1.07),
	('T', 'W', -1.75),
	('T', '-', 0.09),
	('P', 'C', -1.14),
	('P', 'S', -0.52),
	('P', 'T', -0.7),
	('P', 'P', 2.31),
	('P', 'A', -0.24),
	('P', 'G', -1.13),
	('P', 'N', -0.77),
	('P', 'D', -0.75),
	('P', 'E', -0.51),
	('P', 'Q', -0.54),
	('P', 'H', -0.9),
	('P', 'R', -0.87),
	('P', 'K', -0.63),
	('P', 'M', -1.38),
	('P', 'I', -1.35),
	('P', 'L', -1.3),
	('P', 'V', -1.01),
	('P', 'F', -1.76),
	('P', 'Y', -1.52),
	('P', 'W', -2.16),
	('P', '-', 0.36),
	('A', 'C', -0.03),
	('A', 'S', 0.3),
	('A', 'T', -0.05),
	('A', 'P', -0.24),
	('A', 'A', 1.77),
	('A', 'G', -0.42),
	('A', 'N', -0.69),
	('A', 'D', -0.69),
	('A', 'E', -0.34),
	('A', 'Q', -0.32),
	('A', 'H', -0.86),
	('A', 'R', -0.5),
	('A', 'K', -0.38),
	('A', 'M', -0.4),
	('A', 'I', -0.76),
	('A', 'L', -1.07),
	('A', 'V', -0.2),
	('A', 'F', -1.36),
	('A', 'Y', -1.08),
	('A', 'W', -1.82),
	('A', '-', -0.15),
	('G', 'C', -1.87),
	('G', 'S', -0.34),
	('G', 'T', -0.96),
	('G', 'P', -1.13),
	('G', 'A', -0.42),
	('G', 'G', 2.21),
	('G', 'N', 0.1),
	('G', 'D', -0.38),
	('G', 'E', -0.86),
	('G', 'Q', -0.85),
	('G', 'H', -0.96),
	('G', 'R', -1.07),
	('G', 'K', -0.91),
	('G', 'M', -1.72),
	('G', 'I', -2.3),
	('G', 'L', -2.08),
	('G', 'V', -1.98),
	('G', 'F', -2.13),
	('G', 'Y', -1.65),
	('G', 'W', -2.59),
	('G', '-', 0.29),
	('N', 'C', -1.1),
	('N', 'S', 0.08),
	('N', 'T', -0.09),
	('N', 'P', -0.77),
	('N', 'A', -0.69),
	('N', 'G', 0.1),
	('N', 'N', 2.08),
	('N', 'D', 0.52),
	('N', 'E', -0.09),
	('N', 'Q', 0.05),
	('N', 'H', 0.31),
	('N', 'R', -0.28),
	('N', 'K', 0.0),
	('N', 'M', -0.74),
	('N', 'I', -1.36),
	('N', 'L', -1.4),
	('N', 'V', -1.31),
	('N', 'F', -1.17),
	('N', 'Y', -0.49),
	('N', 'W', -2.37),
	('N', '-', 0.36),
	('D', 'C', -1.62),
	('D', 'S', -0.1),
	('D', 'T', -0.3),
	('D', 'P', -0.75),
	('D', 'A', -0.69),
	('D', 'G', -0.38),
	('D', 'N', 0.52),
	('D', 'D', 2.05),
	('D', 'E', 0.59),
	('D', 'Q', -0.11),
	('D', 'H', -0.39),
	('D', 'R', -0.65),
	('D', 'K', -0.27),
	('D', 'M', -1.29),
	('D', 'I', -1.79),
	('D', 'L', -1.7),
	('D', 'V', -1.61),
	('D', 'F', -1.79),
	('D', 'Y', -0.99),
	('D', 'W', -2.3),
	('D', '-', 0.2),
	('E', 'C', -1.51),
	('E', 'S', -0.22),
	('E', 'T', -0.29),
	('E', 'P', -0.51),
	('E', 'A', -0.34),
	('E', 'G', -0.86),
	('E', 'N', -0.09),
	('E', 'D', 0.59),
	('E', 'E', 1.82),
	('E', 'Q', 0.45),
	('E', 'H', -0.27),
	('E', 'R', -0.24),
	('E', 'K', 0.1),
	('E', 'M', -0.97),
	('E', 'I', -1.51),
	('E', 'L', -1.64),
	('E', 'V', -1.09),
	('E', 'F', -1.53),
	('E', 'Y', -1.29),
	('E', 'W', -2.9),
	('E', '-', 0.04),
	('Q', 'C', -1.28),
	('Q', 'S', -0.05),
	('Q', 'T', -0.05),
	('Q', 'P', -0.54),
	('Q', 'A', -0.32),
	('Q', 'G', -0.85),
	('Q', 'N', 0.05),
	('Q', 'D', -0.11),
	('Q', 'E', 0.45),
	('Q', 'Q', 1.87),
	('Q', 'H', 0.2),
	('Q', 'R', 0.41),
	('Q', 'K', 0.42),
	('Q', 'M', -0.25),
	('Q', 'I', -0.9),
	('Q', 'L', -0.83),
	('Q', 'V', -0.8),
	('Q', 'F', -1.05),
	('Q', 'Y', -0.73),
	('Q', 'W', -2.13),
	('Q', '-', 0.15),
	('H', 'C', -0.58),
	('H', 'S', -0.43),
	('H', 'T', -0.57),
	('H', 'P', -0.9),
	('H', 'A', -0.86),
	('H', 'G', -0.96),
	('H', 'N', 0.31),
	('H', 'D', -0.39),
	('H', 'E', -0.27),
	('H', 'Q', 0.2),
	('H', 'H', 2.68),
	('H', 'R', -0.13),
	('H', 'K', -0.06),
	('H', 'M', -0.98),
	('H', 'I', -1.25),
	('H', 'L', -0.98),
	('H', 'V', -1.02),
	('H', 'F', -0.39),
	('H', 'Y', 0.48),
	('H', 'W', -0.92),
	('H', '-', 0.12),
	('R', 'C', -0.96),
	('R', 'S', -0.34),
	('R', 'T', -0.3),
	('R', 'P', -0.87),
	('R', 'A', -0.5),
	('R', 'G', -1.07),
	('R', 'N', -0.28),
	('R', 'D', -0.65),
	('R', 'E', -0.24),
	('R', 'Q', 0.41),
	('R', 'H', -0.13),
	('R', 'R', 1.93),
	('R', 'K', 0.79),
	('R', 'M', -0.44),
	('R', 'I', -1.35),
	('R', 'L', -0.93),
	('R', 'V', -1.09),
	('R', 'F', -1.29),
	('R', 'Y', -0.91),
	('R', 'W', -1.15),
	('R', '-', 0.1),
	('K', 'C', -0.98),
	('K', 'S', -0.21),
	('K', 'T', -0.19),
	('K', 'P', -0.63),
	('K', 'A', -0.38),
	('K', 'G', -0.91),
	('K', 'N', 0.0),
	('K', 'D', -0.27),
	('K', 'E', 0.1),
	('K', 'Q', 0.42),
	('K', 'H', -0.06),
	('K', 'R', 0.79),
	('K', 'K', 1.7),
	('K', 'M', -0.53),
	('K', 'I', -1.13),
	('K', 'L', -1.04),
	('K', 'V', -0.96),
	('K', 'F', -1.42),
	('K', 'Y', -1.03),
	('K', 'W', -1.98),
	('K', '-', 0.11),
	('M', 'C', -0.21),
	('M', 'S', -0.82),
	('M', 'T', -0.18),
	('M', 'P', -1.38),
	('M', 'A', -0.4),
	('M', 'G', -1.72),
	('M', 'N', -0.74),
	('M', 'D', -1.29),
	('M', 'E', -0.97),
	('M', 'Q', -0.25),
	('M', 'H', -0.98),
	('M', 'R', -0.44),
	('M', 'K', -0.53),
	('M', 'M', 2.55),
	('M', 'I', 0.44),
	('M', 'L', 0.77),
	('M', 'V', -0.12),
	('M', 'F', 0.12),
	('M', 'Y', -0.61),
	('M', 'W', -2.16),
	('M', '-', -0.38),
	('I', 'C', -0.63),
	('I', 'S', -1.41),
	('I', 'T', -0.41),
	('I', 'P', -1.35),
	('I', 'A', -0.76),
	('I', 'G', -2.3),
	('I', 'N', -1.36),
	('I', 'D', -1.79),
	('I', 'E', -1.51),
	('I', 'Q', -0.9),
	('I', 'H', -1.25),
	('I', 'R', -1.35),
	('I', 'K', -1.13),
	('I', 'M', 0.44),
	('I', 'I', 1.91),
	('I', 'L', 0.44),
	('I', 'V', 1.01),
	('I', 'F', -0.32),
	('I', 'Y', -0.88),
	('I', 'W', -1.62),
	('I', '-', -0.51),
	('L', 'C', -0.83),
	('L', 'S', -1.28),
	('L', 'T', -0.77),
	('L', 'P', -1.3),
	('L', 'A', -1.07),
	('L', 'G', -2.08),
	('L', 'N', -1.4),
	('L', 'D', -1.7),
	('L', 'E', -1.64),
	('L', 'Q', -0.83),
	('L', 'H', -0.98),
	('L', 'R', -0.93),
	('L', 'K', -1.04),
	('L', 'M', 0.77),
	('L', 'I', 0.44),
	('L', 'L', 1.76),
	('L', 'V', -0.1),
	('L', 'F', 0.21),
	('L', 'Y', -0.73),
	('L', 'W', -1.65),
	('L', '-', -0.31),
	('V', 'C', -0.22),
	('V', 'S', -1.03),
	('V', 'T', -0.19),
	('V', 'P', -1.01),
	('V', 'A', -0.2),
	('V', 'G', -1.98),
	('V', 'N', -1.31),
	('V', 'D', -1.61),
	('V', 'E', -1.09),
	('V', 'Q', -0.8),
	('V', 'H', -1.02),
	('V', 'R', -1.09),
	('V', 'K', -0.96),
	('V', 'M', -0.12),
	('V', 'I', 1.01),
	('V', 'L', -0.1),
	('V', 'V', 1.76),
	('V', 'F', -0.8),
	('V', 'Y', -1.06),
	('V', 'W', -2.01),
	('V', '-', -0.5),
	('F', 'C', -0.39),
	('F', 'S', -1.32),
	('F', 'T', -1.06),
	('F', 'P', -1.76),
	('F', 'A', -1.36),
	('F', 'G', -2.13),
	('F', 'N', -1.17),
	('F', 'D', -1.79),
	('F', 'E', -1.53),
	('F', 'Q', -1.05),
	('F', 'H', -0.39),
	('F', 'R', -1.29),
	('F', 'K', -1.42),
	('F', 'M', 0.12),
	('F', 'I', -0.32),
	('F', 'L', 0.21),
	('F', 'V', -0.8),
	('F', 'F', 2.61),
	('F', 'Y', 1.03),
	('F', 'W', -0.15),
	('F', '-', -0.05),
	('Y', 'C', -0.48),
	('Y', 'S', -0.95),
	('Y', 'T', -1.07),
	('Y', 'P', -1.52),
	('Y', 'A', -1.08),
	('Y', 'G', -1.65),
	('Y', 'N', -0.49),
	('Y', 'D', -0.99),
	('Y', 'E', -1.29),
	('Y', 'Q', -0.73),
	('Y', 'H', 0.48),
	('Y', 'R', -0.91),
	('Y', 'K', -1.03),
	('Y', 'M', -0.61),
	('Y', 'I', -0.88),
	('Y', 'L', -0.73),
	('Y', 'V', -1.06),
	('Y', 'F', 1.03),
	('Y', 'Y', 2.67),
	('Y', 'W', 0.12),
	('Y', '-', -0.06),
	('W', 'C', -2.06),
	('W', 'S', -2.03),
	('W', 'T', -1.75),
	('W', 'P', -2.16),
	('W', 'A', -1.82),
	('W', 'G', -2.59),
	('W', 'N', -2.37),
	('W', 'D', -2.3),
	('W', 'E', -2.9),
	('W', 'Q', -2.13),
	('W', 'H', -0.92),
	('W', 'R', -1.15),
	('W', 'K', -1.98),
	('W', 'M', -2.16),
	('W', 'I', -1.62),
	('W', 'L', -1.65),
	('W', 'V', -2.01),
	('W', 'F', -0.15),
	('W', 'Y', 0.12),
	('W', 'W', 3.78),
	('W', '-', -0.17),
	('-', 'C', -0.38),
	('-', 'S', 0.29),
	('-', 'T', 0.09),
	('-', 'P', 0.36),
	('-', 'A', -0.15),
	('-', 'G', 0.29),
	('-', 'N', 0.36),
	('-', 'D', 0.2),
	('-', 'E', 0.04),
	('-', 'Q', 0.15),
	('-', 'H', 0.12),
	('-', 'R', 0.1),
	('-', 'K', 0.11),
	('-', 'M', -0.38),
	('-', 'I', -0.51),
	('-', 'L', -0.31),
	('-', 'V', -0.5),
	('-', 'F', -0.05),
	('-', 'Y', -0.06),
	('-', 'W', -0.17),
	('-', '-', 0.0)
]).
